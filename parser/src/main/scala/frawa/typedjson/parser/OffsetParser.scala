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
  import frawa.typedjson.parser.{Value => ValueWO}
  sealed trait Value {
    val offset: Offset
  }

  case class NumberValue(offset: Offset, value: BigDecimal)                   extends Value
  case class BoolValue(offset: Offset, value: Boolean)                        extends Value
  case class NullValue(offset: Offset)                                        extends Value
  case class StringValue(offset: Offset, value: CharSequence)                 extends Value
  case class ArrayValue(offset: Offset, vs: Seq[Value])                       extends Value
  case class ObjectValue(offset: Offset, properties: Map[StringValue, Value]) extends Value

  def withoutOffset(value: Value): ValueWO = {
    value match {
      case Offset.NumberValue(_, value) => ValueWO.NumberValue(value)
      case Offset.BoolValue(_, value)   => ValueWO.BoolValue(value)
      case Offset.NullValue(_)          => ValueWO.NullValue
      case Offset.StringValue(_, value) => ValueWO.StringValue(value.toString)
      case Offset.ArrayValue(_, vs)     => ValueWO.ArrayValue(vs.map(withoutOffset))
      case Offset.ObjectValue(_, properties) =>
        ValueWO.ObjectValue(
          properties
            .map { case (key, value) =>
              Value.asString(withoutOffset(key)).map((_, withoutOffset(value)))
            }
            .flatten
            .toMap
        )
    }
  }

}
