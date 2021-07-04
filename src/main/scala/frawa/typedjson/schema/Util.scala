package frawa.typedjson.schema

object Util {

  def sequence[E, T](eithers: Seq[Either[E, T]]): Either[E, Seq[T]] = {
    eithers.foldLeft[Either[E, Seq[T]]](Right[E, Seq[T]](Seq()))((acc, v) => acc.flatMap(acc => v.map(acc :+ _)))
  }

//   def mapNonEmpty[T, S](as: Seq[T])(f: T => S): Option[Seq[S]] =
//     if (as.isEmpty) {
//       None
//     } else {
//       Some(as.map(f))
//     }

  def debugTraceValue[T](title: String): T => T = { v =>
    println(title, v)
    v
  }
}
