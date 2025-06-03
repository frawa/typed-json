/*
 * Copyright 2021 Frank Wagner
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package frawa.typedjson.util

import scala.collection.immutable.Seq

object FP:
  def sequence[A, F[A]](cs: Seq[F[A]])(using monad: Monad[F]): F[Seq[A]] =
    cs.foldLeft(monad.unit(Seq.empty)) { (acc, c) =>
      acc.flatMap(cs => c.map(cs :+ _))
    }

  trait State[S]: // extends Monad[[A] =>> State[A] => (A, State[A])]:
    type S1[A] = S => (A, S)
    def pure[A](a: A): S1[A]                          = s => (a, s)
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
