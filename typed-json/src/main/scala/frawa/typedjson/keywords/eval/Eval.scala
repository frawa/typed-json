package frawa.typedjson.eval

import frawa.typedjson.keywords.Keywords
import frawa.typedjson.keywords.InnerValue
import frawa.typedjson.keywords.Keyword
import frawa.typedjson.pointer.Pointer
import frawa.typedjson.validation.ValidationError
import frawa.typedjson.validation.ValidationAnnotation
import frawa.typedjson.keywords.SchemaProblems

object Eval:
  type EvalFun[O, R[O]]      = InnerValue => R[O]
  type AggregateFun[O, R[O]] = Seq[R[O]] => EvalFun[O, R]

  def compile[O, R[O]](keywords: Keywords)(using proc: Proc[O, R])(using ResultOps[R])(using
      OutputOps[O]
  ): EvalFun[O, R] =
    proc.process(keywords)

trait Proc[O, R[O]]:
  import Keywords.KeywordWithLocation
  import Eval.EvalFun

  def process(keyword: KeywordWithLocation)(using ResultOps[R])(using outops: OutputOps[O]): EvalFun[O, R]

  def process(keywords: Keywords)(using ResultOps[R])(using OutputOps[O]): EvalFun[O, R] = all(
    keywords.map(process)
  )

  protected def all[O, R[O]](es: Seq[EvalFun[O, R]])(using ResultOps[R])(using ops: OutputOps[O]): EvalFun[O, R] =
    (value: InnerValue) =>
      Util
        .sequence(es.map(_(value)))
        .map(ops.all(_))

object Proc:
  given [O, R[O]]: Proc[O, R] = ProcDefault()

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
  extension [O](o: O)
    def not: O
    def isValid: Boolean

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
