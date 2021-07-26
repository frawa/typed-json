package frawa.typedjson.schema

import munit.FunSuite
import frawa.typedjson.parser.Value
import frawa.typedjson.parser.ZioParser
import frawa.typedjson.parser.Parser
import frawa.typedjson.parser.NumberValue
import frawa.typedjson.parser.BoolValue
import frawa.typedjson.parser.NullValue
import frawa.typedjson.parser.StringValue
import frawa.typedjson.parser.ArrayValue
import frawa.typedjson.parser.ObjectValue
import scala.reflect.internal.Reporter

class ProcessorTest extends FunSuite {
  implicit val zioParser = new ZioParser();

  private def withParsedSchemaValue(text: String, parse: String => Either[String, SchemaValue])(
      f: SchemaValue => Unit
  ) {
    val withSchema = for {
      value <- parse(text)
    } yield {
      f(value)
    }
    withSchema.swap
      .map(message => fail("no schema", clues(clue(message))))
      .swap
  }

  private def withSchema(text: String)(f: SchemaValue => Unit) {
    withParsedSchemaValue(text, SchemaValue.apply)(f)
  }

  private def assertValidate(text: String)(schema: SchemaValue)(
      f: ValidationResult => Unit
  ) = {
    val withParsed = for {
      value <- Parser(text)
      result = Processor.process(new ValidationCalculator())(schema, value)
    } yield {
      f(result)
    }
    withParsed.swap
      .map(message => fail("parsing failed", clues(clue(message))))
      .swap
  }

  test("null") {
    withSchema("""{"type": "null"}""") { schema =>
      assertValidate("""null""")(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate("""13""")(schema) { result =>
        assertEquals(result.errors, Seq(WithPointer(TypeMismatch2("null"))))
        assertEquals(result.valid, false)
      }
    }
  }

  test("boolean") {
    withSchema("""{"type": "boolean"}""") { schema =>
      assertValidate("""true""")(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate("""13""")(schema) { result =>
        assertEquals(result.errors, Seq(WithPointer(TypeMismatch2("boolean"))))
        assertEquals(result.valid, false)
      }
    }
  }

  test("true schema") {
    withSchema("""true""") { schema =>
      assertValidate("""null""")(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate("""13""")(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate("""{}""")(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
    }
  }

  test("false schema") {
    withSchema("""false""") { schema =>
      assertValidate("""null""")(schema) { result =>
        assertEquals(
          result.errors,
          Seq(
            WithPointer(
              result = FalseSchemaReason(),
              pointer = Pointer(
                segments = Nil
              )
            )
          )
        )
        assertEquals(result.valid, false)
      }
      assertValidate("""13""")(schema) { result =>
        assertEquals(
          result.errors,
          Seq(
            WithPointer(
              result = FalseSchemaReason(),
              pointer = Pointer(
                segments = Nil
              )
            )
          )
        )
        assertEquals(result.valid, false)
      }
      assertValidate("""{}""")(schema) { result =>
        assertEquals(
          result.errors,
          Seq(
            WithPointer(
              result = FalseSchemaReason(),
              pointer = Pointer(
                segments = Nil
              )
            )
          )
        )
        assertEquals(result.valid, false)
      }
    }
  }

  test("not false") {
    withSchema("""{"not": false}""") { schema =>
      assertValidate("""null""")(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate("""13""")(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate("""{}""")(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
    }
  }

  test("empty schema") {
    withSchema("""{}""") { schema =>
      assertValidate("""null""")(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate("""13""")(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate("""{}""")(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
    }
  }

  test("not empty") {
    withSchema("""{"not": {}}""") { schema =>
      assertValidate("""null""")(schema) { result =>
        assertEquals(
          result.errors,
          Seq(
            WithPointer(
              result = NotInvalid(),
              pointer = Pointer(
                segments = Nil
              )
            )
          )
        )
        assertEquals(result.valid, false)
      }
      assertValidate("""13""")(schema) { result =>
        assertEquals(
          result.errors,
          Seq(
            WithPointer(
              result = NotInvalid(),
              pointer = Pointer(
                segments = Nil
              )
            )
          )
        )
        assertEquals(result.valid, false)
      }
      assertValidate("""{}""")(schema) { result =>
        assertEquals(
          result.errors,
          Seq(
            WithPointer(
              result = NotInvalid(),
              pointer = Pointer(
                segments = Nil
              )
            )
          )
        )
        assertEquals(result.valid, false)
      }
    }
  }

  test("string") {
    withSchema("""{"type": "string"}""") { schema =>
      assertValidate(""""hello"""")(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate("""13""")(schema) { result =>
        assertEquals(result.errors, Seq(WithPointer(TypeMismatch2("string"))))
        assertEquals(result.valid, false)
      }
    }
  }

  test("number") {
    withSchema("""{"type": "number"}""") { schema =>
      assertValidate("""13""")(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate("""null""")(schema) { result =>
        assertEquals(result.errors, Seq(WithPointer(TypeMismatch2("number"))))
        assertEquals(result.valid, false)
      }
    }
  }

  test("array") {
    withSchema("""{"type": "array", "items": { "type": "number"} }""") { schema =>
      assertValidate("""[13]""")(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate("""null""")(schema) { result =>
        assertEquals(result.errors, Seq(WithPointer(TypeMismatch2("array"))))
        assertEquals(result.valid, false)
      }
    }
  }

  test("array item") {
    withSchema("""{"type": "array", "items": { "type": "number"} }""") { schema =>
      assertValidate("""[true]""")(schema) { result =>
        assertEquals(result.errors, Seq(WithPointer(TypeMismatch2("number"), Pointer(0))))
        assertEquals(result.valid, false)
      }
      assertValidate("""[13]""")(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
    }
  }

  test("object") {
    withSchema("""{
                 |"type": "object", 
                 |"properties": { 
                 |  "toto": { "type": "number" },
                 |  "titi": { "type": "string" }
                 |} 
                 |}
                 |""".stripMargin) { schema =>
      assertValidate("""{
                       |"toto": 13,
                       |"titi": "hello"
                       |}
                       |"""".stripMargin)(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate("""null""")(schema) { result =>
        assertEquals(
          result.errors,
          Seq(
            WithPointer(
              TypeMismatch2("object")
            )
          )
        )
        assertEquals(result.valid, false)
      }
    }
  }

  test("object property type") {
    withSchema("""{
                 |"type": "object", 
                 |"properties": { 
                 |  "toto": { "type": "number" },
                 |  "titi": { "type": "string" }
                 |} 
                 |}
                 |""".stripMargin) { schema =>
      assertValidate("""{
                       |"toto": 13,
                       |"titi": true
                       |}
                       |""".stripMargin)(schema) { result =>
        assertEquals(result.errors, Seq(WithPointer(TypeMismatch2("string"), Pointer.empty / "titi")))
        assertEquals(result.valid, false)
      }
    }
  }

  test("object unknown property") {
    withSchema("""{
                 |"type": "object", 
                 |"properties": { 
                 |  "toto": { "type": "number" },
                 |  "titi": { "type": "string" }
                 |} 
                 |}
                 |""".stripMargin) { schema =>
      assertValidate("""{
                       |"gnu": true,
                       |"toto": 13,
                       |"titi": "foo"
                       |}
                       |""".stripMargin)(schema) { result =>
        assertEquals(result.errors, Seq(WithPointer(UnexpectedProperty("gnu"))))
        assertEquals(result.valid, false)
      }
    }
  }

  test("object missing property") {
    withSchema("""{
                 |"type": "object", 
                 |"properties": { 
                 |  "toto": { "type": "number" },
                 |  "titi": { "type": "string" }
                 |} 
                 |}
                 |""".stripMargin) { schema =>
      assertValidate("""{
                       |"toto": 13
                       |}
                       |""".stripMargin)(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
    }
  }

  test("object missing required property") {
    withSchema("""{
                 |"type": "object", 
                 |"properties": { 
                 |  "toto": { "type": "number" },
                 |  "titi": { "type": "string" }
                 |},
                 |"required": ["titi"]
                 |}
                 |""".stripMargin) { schema =>
      assertValidate("""{
                       |"toto": 13
                       |}
                       |""".stripMargin)(schema) { result =>
        assertEquals(
          result.errors,
          Seq(
            WithPointer(
              result = MissingProperties2(
                Map(
                  "titi" -> SchemaValue(
                    value = ObjectValue(
                      properties = Map(
                        "type" -> StringValue(
                          value = "string"
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      }
    }
  }

  test("allOf") {
    withSchema("""{
                 |"allOf": [
                 |  { "type": "number" }
                 |]
                 |}
                 |""".stripMargin) { schema =>
      assertValidate("""1313""".stripMargin)(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
    }
  }

  test("impossible allOf") {
    withSchema("""{
                 |"allOf": [
                 |  { "type": "number" },
                 |  { "type": "string" }
                 |]
                 |}
                 |""".stripMargin) { schema =>
      assertValidate("""1313""".stripMargin)(schema) { result =>
        assertEquals(result.errors, Seq(WithPointer(TypeMismatch2("string"))))
        assertEquals(result.valid, false)
      }
    }
  }

  test("anyOf") {
    withSchema("""{
                 |"anyOf": [
                 |  { "type": "number" },
                 |  { "type": "string" }
                 |]
                 |}
                 |""".stripMargin) { schema =>
      assertValidate("""1313""".stripMargin)(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
    }
  }

  test("failed anyOf") {
    withSchema("""{
                 |"anyOf": [
                 |  { "type": "number" },
                 |  { "type": "string" }
                 |]
                 |}
                 |""".stripMargin) { schema =>
      assertValidate("""true""".stripMargin)(schema) { result =>
        assertEquals(
          result.errors,
          Seq(
            WithPointer(TypeMismatch2("number")),
            WithPointer(TypeMismatch2("string"))
          )
        )
        assertEquals(result.valid, false)
      }
    }
  }

  test("oneOf") {
    withSchema("""{
                 |"anyOf": [
                 |  { "type": "number" },
                 |  { "type": "string" }
                 |]
                 |}
                 |""".stripMargin) { schema =>
      assertValidate("""1313""".stripMargin)(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
    }
  }

  test("failed oneOf: none") {
    withSchema("""{
                 |"oneOf": [
                 |  { "type": "string" },
                 |  { "type": "boolean" }
                 |]
                 |}
                 |""".stripMargin) { schema =>
      assertValidate("""1313""".stripMargin)(schema) { result =>
        assertEquals(
          result.errors,
          Seq(
            WithPointer(TypeMismatch2("string")),
            WithPointer(TypeMismatch2("boolean"))
          )
        )
        assertEquals(result.valid, false)
      }
    }
  }

  test("failed oneOf: two") {
    withSchema("""{
                 |"oneOf": [
                 |  { "type": "number" },
                 |  { "type": "number" }
                 |]
                 |}
                 |""".stripMargin) { schema =>
      assertValidate("""1313""".stripMargin)(schema) { result =>
        assertEquals(
          result.errors,
          Seq(
            WithPointer(NotOneOf(2))
          )
        )
        assertEquals(result.valid, false)

      }
    }
  }
}

// TODO
// - $ref / $defs
