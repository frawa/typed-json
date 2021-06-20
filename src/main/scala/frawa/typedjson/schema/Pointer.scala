package frawa.typedjson.schema

object Pointer {
  def empty = new Pointer(Nil)
}

case class Pointer(segments: Seq[Token]) {
  override def toString(): String = {
    if (this.segments.isEmpty) {
      ""
    } else {
      "/" + this.segments.mkString("/")
    }
  }

  def /(index: Int): Pointer = {
    new Pointer(segments :+ ArrayIndexToken(index))
  }
  def /(field: String): Pointer = {
    new Pointer(segments :+ FieldToken(field))
  }
  def /(pointer: Pointer): Pointer = {
    new Pointer(segments ++ pointer.segments)
  }
}

trait Token
case class ArrayIndexToken(index: Int) extends Token {
  override def toString(): String = { index.toString }
}
case class FieldToken(field: String) extends Token {
  override def toString(): String = {
    field
      .replace("~", "~0")
      .replace("/", "~1")
  }
}
