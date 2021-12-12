package frawa.typedjson.pointer

import frawa.typedjson.parser.Value

trait OffsetParser {
  import Offset._
  def parseWithOffset(json: String): Either[String, WithOffset[Value]]

  def pointerAt(value: WithOffset[Value])(offset: Int): Pointer
  def offsetAt(value: WithOffset[Value])(pointer: Pointer): Offset
}

case class Offset(start: Int, end: Int)
object Offset {
  import frawa.typedjson.parser._

  sealed trait WithOffset[+V] {
    val offset: Offset
    val value: V
  }

  sealed trait ValueWithOffset[+T <: Value] extends WithOffset[T]

  case class NumberValueWithOffset(offset: Offset, value: NumberValue) extends ValueWithOffset[NumberValue]

  case class BoolValueWithOffset(offset: Offset, value: BoolValue) extends ValueWithOffset[BoolValue]

  case class NullValueWithOffset(offset: Offset) extends ValueWithOffset[NullValue.type] {
    val value = NullValue
  }

  case class StringValueWithOffset(offset: Offset, value: StringValue) extends ValueWithOffset[StringValue]

  case class ArrayValueWithOffset(offset: Offset, vs: Seq[ValueWithOffset[Value]]) extends ValueWithOffset[ArrayValue] {
    val value = ArrayValue(vs.map(_.value))
  }

  case class ObjectValueWithOffset(offset: Offset, properties: Map[String, ValueWithOffset[Value]])
      extends ValueWithOffset[ObjectValue] {
    val value = ObjectValue(properties.view.mapValues(_.value).toMap)
  }
}
