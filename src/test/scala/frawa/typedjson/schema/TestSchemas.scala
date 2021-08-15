package frawa.typedjson.schema

import frawa.typedjson.parser.ObjectValue
import frawa.typedjson.parser.StringValue

object TestSchemas {
  val nullSchema         = """{"type": "null"}"""
  val boolSchema         = """{"type": "boolean"}"""
  val trueSchema         = """true"""
  val falseSchema        = """false"""
  val notFalseSchema     = """{"not": false}"""
  val emtpySchema        = """{}"""
  val stringSchema       = """{"type": "string"}"""
  val numberSchema       = """{"type": "number"}"""
  val arraySchema        = """{"type": "array"}"""
  val numberArraySchema  = """{"type": "array", "items": { "type": "number"}}"""
  val totoObjectSchema   = """{
                           |"type": "object",
                           |"properties": {
                           |  "toto": { "type": "number" },
                           |  "titi": { "type": "string" }
                           |}
                           |}
                           |""".stripMargin
  val allOfSchema        = """{
                      |"allOf": [
                      |  { "type": "number" }
                      |]
                      |}
                      |""".stripMargin
  val anyOfSchema        = """{
                      |"anyOf": [
                      |  { "type": "number" },
                      |  { "type": "string" }
                      |]
                      |}
                      |""".stripMargin
  val oneOfSchema        = """{
                      |"oneOf": [
                      |  { "type": "number" },
                      |  { "type": "string" }
                      |]
                      |}
                      |""".stripMargin
  val ifThenElseSchema   = """{
                           |"if": { "type": "number" },
                           |"then": { "type": "number" },
                           |"else": { "type": "string" }
                           |}
                           |""".stripMargin
  val nullOrStringSchema = """{"type": ["null","string"]}"""
  val enumSchema         = """{
                     |"type": "string",
                     |"enum": ["foo", "bar"]
                     |}""".stripMargin
  val constSchema        = """{
                      |"type": "string",
                      |"const": "first"
                      |}""".stripMargin
  val idRefDefsSchema    = """{
                          |"$id": "https://example.net/root.json",
                          |"type": "array",
                          |"items": {
                          |    "$ref": "#item"
                          |},
                          |"$defs": {
                          |    "single": {
                          |        "$anchor": "item",
                          |        "type": "number"
                          |    }
                          |}
                          |}""".stripMargin

  val numberSchemaValue = SchemaValue(
    value = ObjectValue(
      properties = Map(
        "type" -> StringValue(
          value = "number"
        )
      )
    )
  )
  val stringSchemaValue = SchemaValue(
    value = ObjectValue(
      properties = Map(
        "type" -> StringValue(
          value = "string"
        )
      )
    )
  )
}
