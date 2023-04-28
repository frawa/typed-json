package frawa.typedjson.util

object FP:
  def sequence[A, F[A]](cs: Seq[F[A]])(using monad: Monad[F]): F[Seq[A]] =
    cs.foldLeft(monad.unit(Seq.empty)) { (acc, c) =>
      acc.flatMap(cs => c.map(cs :+ _))
    }

  trait State[S]: // extends Monad[[A] =>> State[A] => (A, State[A])]:
    type S1[A] = S => (A, S)
    def pure[A](a: A): S1[A] = s => (a, s)
    def flatMap[A, B](a: S1[A])(f: A => S1[B]): S1[B] = s0 =>
      val (aa, s1) = a(s0)
      f(aa)(s1)

  trait Monad[F[_]] extends Functor[F]:
    def unit[A](x: A): F[A]
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
