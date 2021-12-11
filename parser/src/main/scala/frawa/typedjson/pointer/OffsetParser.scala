package frawa.typedjson.pointer

trait OffsetParser {
  def pointerAt(json: String)(offset: Int): Either[String, Pointer]
}
