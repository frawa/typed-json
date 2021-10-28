package frawa.typedjson.parser

trait Value
case class NumberValue(value: BigDecimal)              extends Value
case class BoolValue(value: Boolean)                   extends Value
case object NullValue                                  extends Value
case class StringValue(value: String)                  extends Value
case class ArrayValue(items: Seq[Value])               extends Value
case class ObjectValue(properties: Map[String, Value]) extends Value

trait Parser {
  def parse(json: String): Either[String, Value]
}

object Parser {
  def apply(json: String)(implicit parser: Parser): Either[String, Value] = {
    parser.parse(json)
  }
}

object Value {
  def asString(value: Value): Option[String] = value match {
    case StringValue(v) => Some(v)
    case _              => None
  }

  def asStrings(values: Seq[Value]): Seq[String] = values.flatMap(asString)
}
