package frawa.typedjson.schema

import frawa.typedjson.parser.Value
import frawa.typedjson.parser.ArrayValue
import frawa.typedjson.parser.ObjectValue

object Pointer {
  def empty                         = new Pointer(Nil)
  def apply(index: Int): Pointer    = Pointer.empty / index
  def apply(field: String): Pointer = Pointer.empty / field

  def parse(spec: String): Pointer = Pointer( // TODO ArrayIndexToken?
    spec
      .split("/")
      .map(field =>
        field
          .replace("~1", "/")
          .replace("~0", "~")
      )
      .filter(_.length() > 0)
      .map(t => t.toIntOption.map(ArrayIndexToken(_)).getOrElse(FieldToken(t)))
  )

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

  def apply(value: Value): Option[Value] = segments.foldLeft(Option(value)) { case (v, segment) =>
    v.flatMap(segment(_))
  }
}

trait Token {
  def apply(value: Value): Option[Value]
}

case class ArrayIndexToken(index: Int) extends Token {
  override def toString(): String = { index.toString }
  override def apply(value: Value): Option[Value] = value match {
    case ArrayValue(values) =>
      if (values.isDefinedAt(index)) {
        Some(values.apply(index))
      } else {
        None
      }
    case ObjectValue(properties) => properties.get(index.toString)
    case _                       => None
  }
}
case class FieldToken(field: String) extends Token {
  override def toString(): String = {
    field
      .replace("~", "~0")
      .replace("/", "~1")
  }
  override def apply(value: Value): Option[Value] = value match {
    case ObjectValue(properties) => properties.get(field)
    case _                       => None
  }
}
