package frawa.typedjson.schema

import munit.FunSuite
import frawa.typedjson.parser.ZioParser
import frawa.typedjson.parser._
import TestUtil._

class ValidationCheckerTest extends FunSuite {
  implicit val zioParser = new ZioParser();

  private def assertValidate(text: String)(schema: SchemaValue)(
      f: ValidationResult => Unit
  ) = {
    val withParsed = for {
      value     <- Parser(text)
      processor <- Processor(schema)(new ValidationChecker())
      result = processor.process(value)
    } yield {
      f(result)
    }
    withParsed.swap
      .map(message => fail("parsing failed", clues(clue(message))))
      .swap
  }

  test("null".only) {
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

  test("boolean".only) {
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

  test("true schema".only) {
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

  test("false schema".only) {
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

  test("not false".only) {
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

  test("empty schema".only) {
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

  test("not empty".only) {
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

  test("string".only) {
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

  test("number".only) {
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

  test("array".only) {
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

  test("array item".only) {
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

  test("object".only) {
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

  test("object property type".only) {
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

  test("object unknown property".only) {
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

  test("object missing property".only) {
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

  test("object missing required property".only) {
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
                    value = NullValue
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

  test("not") {
    withSchema("""{
                 |"not": { "type": "number" }
                 |}
                 |""".stripMargin) { schema =>
      assertValidate("""true""".stripMargin)(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
    }
  }

  test("failed not") {
    withSchema("""{
                 |"not": { "type": "number" }
                 |}
                 |""".stripMargin) { schema =>
      assertValidate("""1313""".stripMargin)(schema) { result =>
        assertEquals(result.errors, Seq(WithPointer(NotInvalid())))
        assertEquals(result.valid, false)
      }
    }
  }

  test("if/then/else") {
    withSchema("""{
                 |"if": { "type": "number" },
                 |"then": { "type": "number" },
                 |"else": { "type": "string" }
                 |}
                 |""".stripMargin) { schema =>
      assertValidate("""1313""".stripMargin)(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate(""""string"""".stripMargin)(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate("""null""".stripMargin)(schema) { result =>
        assertEquals(result.errors, Seq(WithPointer(TypeMismatch2("string"))))
        assertEquals(result.valid, false)
      }
    }
  }

  test("then/if/else") {
    withSchema("""{
                 |"then": { "type": "number" },
                 |"if": { "type": "number" },
                 |"else": { "type": "string" }
                 |}
                 |""".stripMargin) { schema =>
      assertValidate("""1313""".stripMargin)(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate(""""string"""".stripMargin)(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate("""null""".stripMargin)(schema) { result =>
        assertEquals(result.errors, Seq(WithPointer(TypeMismatch2("string"))))
        assertEquals(result.valid, false)
      }
    }
  }

  test("if/else") {
    withSchema("""{
                 |"if": { "type": "number" },
                 |"else": { "type": "string" }
                 |}
                 |""".stripMargin) { schema =>
      assertValidate("""1313""".stripMargin)(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate(""""string"""".stripMargin)(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate("""null""".stripMargin)(schema) { result =>
        assertEquals(result.errors, Seq(WithPointer(TypeMismatch2("string"))))
        assertEquals(result.valid, false)
      }
    }
  }

  test("if/then") {
    withSchema("""{
                 |"if": { "type": "number" },
                 |"then": { "type": "number" }
                 |}
                 |""".stripMargin) { schema =>
      assertValidate("""1313""".stripMargin)(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate(""""string"""".stripMargin)(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
    }
  }

  test("then/else") {
    withSchema("""{
                 |"then": { "type": "number" },
                 |"else": { "type": "string" }
                 |}
                 |""".stripMargin) { schema =>
      assertValidate("""1313""".stripMargin)(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate(""""string"""".stripMargin)(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate("""null""".stripMargin)(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
    }
  }

  test("null or string") {
    withSchema("""{"type": ["null","string"]}""") { schema =>
      assertValidate("""null""")(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate(""""hello"""")(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate("""13""")(schema) { result =>
        assertEquals(
          result.errors,
          Seq(
            WithPointer(TypeMismatch2("null")),
            WithPointer(TypeMismatch2("string"))
          )
        )
        assertEquals(result.valid, false)
      }
    }
  }

  test("enum") {
    withSchema("""{
                 |"type": "string",
                 |"enum": ["foo", "bar"]
                 |}""".stripMargin) { schema =>
      assertValidate(""""foo"""")(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate(""""bar"""")(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate(""""hello"""")(schema) { result =>
        assertEquals(
          result.errors,
          Seq(
            WithPointer(
              NotInEnum(
                Seq(
                  StringValue("foo"),
                  StringValue("bar")
                )
              )
            )
          )
        )
        assertEquals(result.valid, false)
      }
    }
  }

  test("const") {
    withSchema("""{
                 |"type": "string",
                 |"const": "first"
                 |}""".stripMargin) { schema =>
      assertValidate(""""first"""")(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate("""{}""".stripMargin)(schema) { result =>
        assertEquals(
          result.errors,
          Seq(
            WithPointer(
              result = TypeMismatch2(
                expected = "string"
              ),
              pointer = Pointer(
                segments = Nil
              )
            ),
            WithPointer(
              result = NotInEnum(
                values = List(
                  StringValue(
                    value = "first"
                  )
                )
              ),
              pointer = Pointer(
                segments = Nil
              )
            )
          )
        )
      }
      assertValidate(""""second"""")(schema) { result =>
        assertEquals(
          result.errors,
          Seq(
            WithPointer(
              result = NotInEnum(
                values = List(
                  StringValue(
                    value = "first"
                  )
                )
              ),
              pointer = Pointer(
                segments = Nil
              )
            )
          )
        )
      }
    }
  }

  test("$id/$ref/$def".ignore) {
    withSchema("""{
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
                 |}""".stripMargin) { schema =>
      assertValidate("""[1313]""".stripMargin)(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate(""""string"""".stripMargin)(schema) { result =>
        assertEquals(
          result.errors,
          Seq(
            WithPointer(
              result = TypeMismatch2(
                expected = "array"
              ),
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
}

/*
{
    "$id": "https://example.net/root.json",
    "items": {
        "type": "array",
        "items": { "$ref": "#item" }
    },
    "$defs": {
        "single": {
            "$anchor": "item",
            "type": "object",
            "additionalProperties": { "$ref": "other.json" }
        }
    }
}
{
    "$id": "https://example.net/root.json",
    "type": "array",
    "items": {
        "$ref": "#item"
    },
    "$defs": {
        "single": {
            "$anchor": "item",
            "type": "number"
        }
    }
}
 */
