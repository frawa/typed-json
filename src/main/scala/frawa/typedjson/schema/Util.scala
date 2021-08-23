package frawa.typedjson.schema

import frawa.typedjson.parser.Value
import frawa.typedjson.parser.StringValue

object Util {

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

  def toStrings(values: Seq[Value]): Seq[String] = values.flatMap {
    case StringValue(v) => Some(v)
    case _              => None
  }

  def debugTraceValue[T](title: String): T => T = { v =>
    println(title, v)
    v
  }
}
