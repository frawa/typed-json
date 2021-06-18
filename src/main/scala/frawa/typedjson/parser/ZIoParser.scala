package frawa.typedjson.parser

import zio.json.ast.Json
import zio.json._
import zio.json.ast.Json.Arr
import zio.json.ast.Json.Obj

class ZioParser extends Parser {

  override def parse(json: String): Either[String, Value] = {
    val result = json.fromJson[Json]
    result.map(toValue)
  }

  private def toValue(ast: Json): Value = {
    ast match {
      case Json.Num(value)  => NumberValue(value)
      case Json.Bool(value) => BoolValue(value)
      case Json.Null        => NullValue
      case Json.Str(value)  => StringValue(value)
      case Arr(elements)    => ArrayValue(elements.map(toValue))
      case Obj(fields)      => ObjectValue(Map.from(fields.map(p => (p._1, toValue(p._2)))))
    }
  }

}
