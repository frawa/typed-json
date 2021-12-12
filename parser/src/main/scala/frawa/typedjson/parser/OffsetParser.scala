package frawa.typedjson.parser

import frawa.typedjson.pointer.Pointer

trait OffsetParser {
  import Offset._
  def parseWithOffset(json: String): Either[String, Value]

  def pointerAt(value: Value)(offset: Int): Pointer
  def offsetAt(value: Value)(pointer: Pointer): Offset
}

case class Offset(start: Int, end: Int)
object Offset {

  sealed trait Value {
    val offset: Offset
  }

  case class NumberValue(offset: Offset, value: BigDecimal)              extends Value
  case class BoolValue(offset: Offset, value: Boolean)                   extends Value
  case class NullValue(offset: Offset)                                   extends Value
  case class StringValue(offset: Offset, value: CharSequence)            extends Value
  case class ArrayValue(offset: Offset, vs: Seq[Value])                  extends Value
  case class ObjectValue(offset: Offset, properties: Map[String, Value]) extends Value
}
