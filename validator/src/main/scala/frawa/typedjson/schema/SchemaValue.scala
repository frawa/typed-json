package frawa.typedjson.schema

import frawa.typedjson.parser.{StringValue, Value}

case class SchemaValue(value: Value)

object SchemaValue {
  def id(schema: SchemaValue): Option[String] = {
    (Pointer.empty / "$id")(schema.value).flatMap {
      case StringValue(id) => Some(id)
      case _               => None
    }
  }
}
