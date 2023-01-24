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

trait TheResultMonad[R[_]]:
  def unit[A](a: A): R[A]
  def flatMap[A, B](a: R[A])(f: A => R[B]): R[B]
  def map[A, B](a: R[A])(f: A => B): R[B] = flatMap(a)(a => unit(f(a)))
  def output[O](result: R[O]): O
end TheResultMonad

object Eval:
  type Fun[O] = Value => O

class Eval[R[_]: TheResultMonad, O: OutputOps](using Proc[O]):
  import Eval.Fun
  import Keywords.KeywordWithLocation
  // type AggregateFun[O, R[O]] = Seq[R[O]] => EvalFun[O, R]

  protected val monad = summon[TheResultMonad[R]]
  protected val proc  = summon[Proc[O]]

  final def eval(keyword: Keyword)(value: Value): O =
    val compiled = compile(keyword)
    monad.output(eval(compiled, value))

  final def compile(keyword: Keyword): R[Fun[O]] = evalOne(keyword)
  final def compile(keywords: Keywords): R[Fun[O]] =
    monad.map(compile(keywords.keywords.toSeq))(fs => proc.validateAll(fs))
  final def compile(keywords: Seq[KeywordWithLocation]): R[Seq[Fun[O]]] =
    val fs = keywords.map(_.value).map(compile)
    fs.foldLeft(monad.unit(Seq())) { (acc, f) =>
      monad.flatMap(acc)(acc => monad.flatMap(f)(f => monad.unit(acc :+ f)))
    }

  final def eval(compiled: R[Fun[O]], value: Value): R[O] = monad.map(compiled)(f => f(value))

  private def evalOne(k: Keyword): R[Fun[O]] = // value => ops.valid
    k match {
      case NullTypeKeyword    => monad.unit(proc.validateType(proc.nullTypeMismatch))
      case BooleanTypeKeyword => monad.unit(proc.validateType(proc.booleanTypeMismatch))
      // ...
      case NotKeyword(ks)       => monad.map(compile(ks))(f => proc.validateNot(f))
      case UnionTypeKeyword(ks) => monad.map(compile(ks))(fs => proc.validateUnion(fs))
    }

trait Proc[O: OutputOps]:
  import Eval.Fun

  def process(keyword: Keyword): Value => O

  def validateType[T <: Value](error: TypeMismatch[T])(using TypeTest[Value, T]): Fun[O]

  def validateNot(f: Fun[O]): Fun[O]
  def validateUnion(fs: Seq[Fun[O]]): Fun[O]
  def validateAll(fs: Seq[Fun[O]]): Fun[O]

  val nullTypeMismatch    = TypeMismatch[NullValue.type]("null")
  val booleanTypeMismatch = TypeMismatch[BoolValue]("boolean")
  val stringTypeMismatch  = TypeMismatch[StringValue]("string")
  val numberTypeMismatch  = TypeMismatch[NumberValue]("number")
  val arrayTypeMismatch   = TypeMismatch[ArrayValue]("array")
  val objectTypeMismatch  = TypeMismatch[ObjectValue]("object")

  // def process(keywords: Keywords)(using ResultOps[R])(using OutputOps[O]): EvalFun[O, R] = all(
  //   keywords.map(process)
  // )

  // protected def all[O, R[O]](es: Seq[EvalFun[O, R]])(using ResultOps[R])(using ops: OutputOps[O]): EvalFun[O, R] =
  //   (value: InnerValue) =>
  //     Util
  //       .sequence(es.map(_(value)))
  //       .map(ops.all(_))

// object Proc:
// given [O, R[O]]: Proc[O, R] = ProcDefault()

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
