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
  val totoObjectSchema  = """{
                           |"type": "object",
                           |"properties": {
                           |  "toto": { "type": "number" },
                           |  "titi": { "type": "string" }
                           |}
                           |}
                           |""".stripMargin

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
