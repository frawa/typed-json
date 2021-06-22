package frawa.typedjson.schema

import zio.json.ast.Json
import zio.json._
import zio.json.ast.Json.Arr
import zio.json.ast.Json.Obj

class ZioSchemaParser extends SchemaParser {
  import SchemaDecoders._

  override def parse(json: String): Either[String, Schema] = {
    json.fromJson[Schema]
  }
}

object SchemaDecoders {
  implicit val schemaDecoder: JsonDecoder[Schema] =
    JsonDecoder
      .hashMap[String, Json]
      .mapOrFail { fields =>
        fields
          .get("type")
          .map { t =>
            t match {
              case Json.Str("null")    => Right(NullSchema)
              case Json.Str("boolean") => Right(BooleanSchema)
              case Json.Str("string")  => Right(StringSchema)
              case Json.Str("number")  => Right(NumberSchema)
              case Json.Str("array") =>
                fields
                  .get("items")
                  .map(_.as[Schema])
                  .map(items => items.map(ArraySchema(_)))
                  .getOrElse((Left(s"missing 'items'")))
              case Json.Str("object") =>
                fields
                  .get("properties")
                  .map(_.as[Map[String, Schema]])
                  .map(items => items.map(ObjectSchema(_)))
                  .getOrElse((Left(s"missing 'properties'")))
              case _ => Left(s"unknown type '${t}'")
            }
          }
          .getOrElse(
            if (fields.isEmpty)
              Right(TrueSchema)
            else
              Left("missing 'type'")
          )
      }
      .orElse {
        JsonDecoder.boolean.map(v => if (v) TrueSchema else FalseSchema)
      }
}
