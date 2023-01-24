package frawa.typedjson.eval

import frawa.typedjson.keywords.Keywords
import frawa.typedjson.keywords.InnerValue
import frawa.typedjson.keywords.Keyword
import frawa.typedjson.pointer.Pointer
import frawa.typedjson.validation.ValidationError
import frawa.typedjson.validation.ValidationAnnotation
import frawa.typedjson.keywords.SchemaProblems
import frawa.typedjson.keywords.BooleanTypeKeyword
import frawa.typedjson.validation.TypeMismatch
import frawa.typedjson.parser.Value
import frawa.typedjson.parser.Value._
import frawa.typedjson.keywords.NullTypeKeyword
import frawa.typedjson.keywords.NotKeyword
import frawa.typedjson.keywords.UnionTypeKeyword
import scala.reflect.TypeTest
import frawa.typedjson.keywords.TrivialKeyword

trait TheResultMonad[R[_]]:
  def unit[A](a: A): R[A]
  def flatMap[A, B](a: R[A])(f: A => R[B]): R[B]
  def map[A, B](a: R[A])(f: A => B): R[B] = flatMap(a)(a => unit(f(a)))
  def output[O](result: R[O]): O
end TheResultMonad

object Eval:
  type Fun[O] = Value => O

class Eval[R[_]: TheResultMonad, O: OutputOps]:
  import Eval.Fun
  import Keywords.KeywordWithLocation
  // type AggregateFun[O, R[O]] = Seq[R[O]] => EvalFun[O, R]

  protected val monad  = summon[TheResultMonad[R]]
  protected val verify = Verify[O]

  final def eval(keyword: Keyword)(value: Value): O =
    val compiled = compile(keyword)
    monad.output(eval(compiled, value))

  final def compile(keyword: Keyword): R[Fun[O]] = evalOne(keyword)
  final def compile(keywords: Keywords): R[Fun[O]] =
    monad.map(compile(keywords.keywords.toSeq))(fs => verify.verifyAll(fs))
  final def compile(keywords: Seq[KeywordWithLocation]): R[Seq[Fun[O]]] =
    val fs = keywords.map(_.value).map(compile)
    fs.foldLeft(monad.unit(Seq())) { (acc, f) =>
      monad.flatMap(acc)(acc => monad.flatMap(f)(f => monad.unit(acc :+ f)))
    }

  final def eval(compiled: R[Fun[O]], value: Value): R[O] = monad.map(compiled)(f => f(value))

  private def evalOne(k: Keyword): R[Fun[O]] =
    k match {
      case NullTypeKeyword    => monad.unit(verify.verifyType(verify.nullTypeMismatch))
      case TrivialKeyword(v)  => monad.unit(verify.verifyTrivial(v))
      case BooleanTypeKeyword => monad.unit(verify.verifyType(verify.booleanTypeMismatch))
      // ...
      case NotKeyword(ks)       => monad.map(compile(ks))(f => verify.verifyNot(f))
      case UnionTypeKeyword(ks) => monad.map(compile(ks))(fs => verify.verifyUnion(fs))
    }

trait OutputOps[O] extends Monoid[O]:
  def valid: O
  def valid(annotation: ValidationAnnotation, pointer: Pointer): O
  def invalid(error: ValidationError, pointer: Pointer): O
  def invalid(problems: SchemaProblems): O

  def all(os: Seq[O]): O
  def any(os: Seq[O]): O
  def one(os: Seq[O]): O
  def contains(os: Seq[O], min: Option[Int], max: Option[Int], pointer: Pointer): O

  def unit = valid
  extension (o: O)
    def not: O
    def isValid: Boolean
    def combine(o2: O): O = all(Seq(o, o2))

trait ResultOps[R[_]] extends Monad[R]

object Util:
  def sequence[A, F[A]](cs: Seq[F[A]])(using monad: Monad[F]): F[Seq[A]] =
    cs.foldLeft(monad.pure(Seq.empty)) { (acc, c) =>
      acc.flatMap(cs => c.map(cs :+ _))
    }

trait State[S]: // extends Monad[[A] =>> State[A] => (A, State[A])]:
  type S1[A] = S => (A, S)

  def pure[A](a: A): S1[A] = s => (a, s)
  def flatMap[A, B](a: S1[A])(f: A => S1[B]): S1[B] = s0 =>
    val (aa, s1) = a(s0)
    f(aa)(s1)

trait Monad[F[_]] extends Functor[F]:

  /** The unit value for a monad */
  def pure[A](x: A): F[A]

  extension [A](x: F[A])
    /** The fundamental composition operation */
    def flatMap[B](f: A => F[B]): F[B]

  /** The `map` operation can now be defined in terms of `flatMap` */
  // def map[B](f: A => B) = x.flatMap(f.andThen(pure))

trait Functor[F[_]]:
  extension [A](x: F[A]) def map[B](f: A => B): F[B]

trait SemiGroup[T]:
  extension (x: T) def combine(y: T): T

trait Monoid[T] extends SemiGroup[T]:
  def unit: T
