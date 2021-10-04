package frawa.typedjson.schema

import frawa.typedjson.parser.ObjectValue
import frawa.typedjson.parser.StringValue

object TestSchemas {
  val nullSchema        = """{"type": "null"}"""
  val boolSchema        = """{"type": "boolean"}"""
  val trueSchema        = """true"""
  val falseSchema       = """false"""
  val notFalseSchema    = """{"not": false}"""
  val emtpySchema       = """{}"""
  val stringSchema      = """{"type": "string"}"""
  val numberSchema      = """{"type": "number"}"""
  val arraySchema       = """{"type": "array"}"""
  val numberArraySchema = """{"type": "array", "items": { "type": "number"}}"""

  val totoObjectSchema = """{
                           |"type": "object",
                           |"properties": {
                           |  "toto": { "type": "number" },
                           |  "titi": { "type": "string" }
                           |}
                           |}
                           |""".stripMargin

  val totoRequiredObjectSchema = """{
                                   |"type": "object",
                                   |"required": ["toto", "gnu"],
                                   |"properties": {
                                   |  "toto": { "type": "number" },
                                   |  "gnu": { "type": "boolean" },
                                   |  "titi": { "type": "string" }
                                   |}
                                   |}
                                   |""".stripMargin

  val allOfSchema = """{
                      |"allOf": [
                      |  { "type": "number" }
                      |]
                      |}
                      |""".stripMargin

  val anyOfSchema = """{
                      |"anyOf": [
                      |  { "type": "number" },
                      |  { "type": "string" }
                      |]
                      |}
                      |""".stripMargin

  val oneOfSchema = """{
                      |"oneOf": [
                      |  { "type": "number" },
                      |  { "type": "string" }
                      |]
                      |}
                      |""".stripMargin

  val ifThenElseSchema = """{
                           |"if": { "type": "number" },
                           |"then": { "type": "number" },
                           |"else": { "type": "string" }
                           |}
                           |""".stripMargin

  val nullOrStringSchema = """{"type": ["null","string"]}"""

  val enumSchema  = """{
                     |"type": "string",
                     |"enum": ["foo", "bar"]
                     |}""".stripMargin
  val constSchema = """{
                      |"type": "string",
                      |"const": "first"
                      |}""".stripMargin

  val idRefDefsSchema = """{
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

  val recursiveRefDefsSchema = """|{
                                  |"$ref": "#/$defs/list",
                                  |"$defs": {
                                  |  "list": {
                                  |    "oneOf": [
                                  |      { "$ref": "#/$defs/list" },
                                  |      { "type": "string" }
                                  |    ]
                                  |  }
                                  |}
                                  |}
                                  |""".stripMargin

  val subItemRefDefsSchema = """|{"$defs": {
                                |"item": {
                                |  "type": "array",
                                |  "items": false,
                                |  "prefixItems": [
                                |    { "$ref": "#/$defs/sub-item" },
                                |    { "$ref": "#/$defs/sub-item" }
                                |  ]
                                |},
                                |"sub-item": {
                                |  "type": "object",
                                |  "required": ["foo"]
                                |}
                                |},
                                |"type": "array",
                                |"items": false,
                                |"prefixItems": [
                                |  { "$ref": "#/$defs/item" },
                                |  { "$ref": "#/$defs/item" },
                                |  { "$ref": "#/$defs/item" }
                                |]
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
