package frawa.typedjson.schema

import frawa.typedjson.parser.Value
import frawa.typedjson.parser.NullValue
import frawa.typedjson.parser.ObjectValue

trait Schema

case object NullSchema                                   extends Schema
case object TrueSchema                                   extends Schema
case object FalseSchema                                  extends Schema
case object BooleanSchema                                extends Schema
case object StringSchema                                 extends Schema
case object NumberSchema                                 extends Schema
case class ArraySchema(items: Schema)                    extends Schema
case class ObjectSchema(properties: Map[String, Schema]) extends Schema

trait SchemaParser {
  def parse(json: String): Either[String, Schema]
}

object SchemaParser {
  def apply(json: String)(implicit parser: SchemaParser): Either[String, Schema] = parser.parse(json)
}
