package frawa.typedjson.util

object Debug:
  def debugTraceValue[T](title: String): T => T = v =>
    println(s"${title}: ${v}")
    v
