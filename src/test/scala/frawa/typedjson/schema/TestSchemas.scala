package frawa.typedjson.schema

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
}
