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

object SeqUtil {

  def sequenceFirstLeft[E, T](eithers: Seq[Either[E, T]]): Either[E, Seq[T]] = {
    eithers.foldLeft[Either[E, Seq[T]]](Right[E, Seq[T]](Seq()))((acc, v) => acc.flatMap(acc => v.map(acc :+ _)))
  }

  def sequenceAllLefts[E, V](as: Seq[Either[Seq[E], V]]): Either[Seq[E], Seq[V]] = {
    as.foldLeft[Either[Seq[E], Seq[V]]](Right(Seq.empty[V])) {
      case (Right(acc), Right(v))    => Right(acc :+ v)
      case (Right(_), Left(errors))  => Left(errors)
      case (Left(acc), Left(errors)) => Left(acc :++ errors)
      case (Left(acc), _)            => Left(acc)
    }
  }

  def debugTraceValue[T](title: String): T => T = { v =>
    println(title, v)
    v
  }
}
