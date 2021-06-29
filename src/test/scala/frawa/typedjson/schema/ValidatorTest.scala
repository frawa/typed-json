package frawa.typedjson.schema

import munit._
import frawa.typedjson.parser.ZioParser
import frawa.typedjson.parser.Parser

// see https://json-schema.org/draft/2020-12/json-schema-core.html

class ValidatorTest extends FunSuite {
  implicit val zioParser    = new ZioParser();
  implicit val schemaParser = SchemaValueDecoder;

  private def testParsedSchema(text: String, parse: String => Either[String, Schema])(f: Schema => Unit) {
    val withSchema = for {
      schema <- parse(text)
    } yield {
      f(schema)
    }
    withSchema.swap
      .map(message => fail("no schema", clues(clue(message))))
      .swap
  }

  private def testSchema(text: String)(f: Schema => Unit) {
    testParsedSchema(text, SchemaParser.schema)(f)
  }

  private def testRootSchema(text: String)(f: Schema => Unit) {
    testParsedSchema(text, SchemaParser.apply)(f)
  }

  private def assertValidate(text: String)(schema: Schema)(
      f: ValidationResult => Unit
  ) = {
    val withParsed = for {
      value <- Parser(text)
      result = Validator.validate(schema)(value)
    } yield {
      f(result)
    }
    withParsed.swap
      .map(message => fail("parsing failed", clues(clue(message))))
      .swap
  }

  test("null") {
    testSchema("""{"type": "null"}""") { schema =>
      assertValidate("""null""")(schema) { result =>
        assertEquals(result.valid, true)
      }
      assertValidate("""13""")(schema) { result =>
        assertEquals(result.errors, Seq(ValidationError(TypeMismatch("null"))))
      }
    }
  }

  test("boolean") {
    testSchema("""{"type": "boolean"}""") { schema =>
      assertValidate("""true""")(schema) { result =>
        assertEquals(result.valid, true)
      }
      assertValidate("""13""")(schema) { result =>
        assertEquals(result.errors, Seq(ValidationError(TypeMismatch("boolean"))))
      }
    }
  }

  test("boolean false") {
    testSchema("""{"type": "boolean"}""") { schema =>
      assertValidate("""false""")(schema) { result =>
        assertEquals(result.errors, Seq(ValidationError(FalseSchemaReason())))
      }
    }
  }

  test("string") {
    testSchema("""{"type": "string"}""") { schema =>
      assertValidate(""""hello"""")(schema) { result =>
        assertEquals(result.valid, true)
      }
      assertValidate("""13""")(schema) { result =>
        assertEquals(result.errors, Seq(ValidationError(TypeMismatch("string"))))
      }
    }
  }

  test("number") {
    testSchema("""{"type": "number"}""") { schema =>
      assertValidate("""13""")(schema) { result =>
        assertEquals(result.valid, true)
      }
      assertValidate("""null""")(schema) { result =>
        assertEquals(result.errors, Seq(ValidationError(TypeMismatch("number"))))
      }
    }
  }

  test("array") {
    testSchema("""{"type": "array", "items": { "type": "number"} }""") { schema =>
      assertValidate("""[13]""")(schema) { result =>
        assertEquals(result.valid, true)
      }
      assertValidate("""null""")(schema) { result =>
        assertEquals(result.errors, Seq(ValidationError(TypeMismatch("array"))))
      }
    }
  }

  test("array item") {
    testSchema("""{"type": "array", "items": { "type": "number"} }""") { schema =>
      assertValidate("""[true]""")(schema) { result =>
        assertEquals(result.errors, Seq(ValidationError(TypeMismatch("number"), Pointer(0))))
      }
    }
  }

  test("object") {
    testSchema("""{
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
        assertEquals(result.valid, true)
      }
      assertValidate("""null""")(schema) { result =>
        assertEquals(result.errors, Seq(ValidationError(TypeMismatch("object"))))
      }
    }
  }

  test("object property type") {
    testSchema("""{
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
        assertEquals(result.errors, Seq(ValidationError(TypeMismatch("string"), Pointer.empty / "titi")))
      }
    }
  }

  test("object unknown property") {
    testSchema("""{
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
        assertEquals(result.errors, Seq(ValidationError(UnexpectedProperty("gnu"))))
      }
    }
  }

  test("object missing property") {
    testSchema("""{
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
        assertEquals(result.errors, Seq(ValidationError(MissingProperty("titi"))))
      }
    }
  }

  test("true schema") {
    testSchema("""true""".stripMargin) { schema =>
      assertValidate("""{
                       |"toto": 13
                       |}
                       |""".stripMargin)(schema) { result =>
        assertEquals(result.valid, true)
      }
    }
  }

  test("empty schema") {
    testSchema("""{}""".stripMargin) { schema =>
      assertValidate("""{
                       |"toto": 13
                       |}
                       |""".stripMargin)(schema) { result =>
        assertEquals(result.valid, true)
      }
    }
  }

  test("false schema") {
    testSchema("""false""".stripMargin) { schema =>
      assertValidate("""{
                       |"toto": 13
                       |}
                       |""".stripMargin)(schema) { result =>
        assertEquals(result.valid, false)
        assertEquals(result.errors, Seq(ValidationError(FalseSchemaReason())))
      }
    }
  }

  test("$ref schema") {
    testSchema("""{
                 |"$ref": "#/$defs/toto"
                 |}
                 |""".stripMargin) { schema =>
      assertValidate("""true""".stripMargin)(schema) { result =>
        assertEquals(result.valid, false)
        assertEquals(result.errors, Seq(ValidationError(MissingRef("#/$defs/toto"))))
      }
    }
  }

  test("dereference $ref schema") {
    testRootSchema("""{
                     |"$id": "id13",
                     |"$ref": "#/$defs/toto",
                     |"$defs": {
                     |  "toto": { "type": "number" }
                     |}
                     |}
                     |""".stripMargin) { schema =>
      assertValidate("""1313""".stripMargin)(schema) { result =>
        assertEquals(result.valid, true)
      }
    }
  }

  test("allOf") {
    testSchema("""{
                 |"allOf": [
                 |  { "type": "number" }
                 |]
                 |}
                 |""".stripMargin) { schema =>
      assertValidate("""1313""".stripMargin)(schema) { result =>
        assertEquals(result.valid, true)
      }
    }
  }

  test("impossible allOf") {
    testSchema("""{
                 |"allOf": [
                 |  { "type": "number" },
                 |  { "type": "string" }
                 |]
                 |}
                 |""".stripMargin) { schema =>
      assertValidate("""1313""".stripMargin)(schema) { result =>
        assertEquals(result.valid, false)
        assertEquals(result.errors, Seq(ValidationError(TypeMismatch("string"))))
      }
    }
  }

  test("anyOf") {
    testSchema("""{
                 |"anyOf": [
                 |  { "type": "number" },
                 |  { "type": "string" }
                 |]
                 |}
                 |""".stripMargin) { schema =>
      assertValidate("""1313""".stripMargin)(schema) { result =>
        assertEquals(result.valid, true)
      }
    }
  }

  test("failed anyOf") {
    testSchema("""{
                 |"anyOf": [
                 |  { "type": "number" },
                 |  { "type": "string" }
                 |]
                 |}
                 |""".stripMargin) { schema =>
      assertValidate("""true""".stripMargin)(schema) { result =>
        assertEquals(result.valid, false)
        assertEquals(
          result.errors,
          Seq(
            ValidationError(TypeMismatch("number")),
            ValidationError(TypeMismatch("string"))
          )
        )
      }
    }
  }

  test("oneOf") {
    testSchema("""{
                 |"anyOf": [
                 |  { "type": "number" },
                 |  { "type": "string" }
                 |]
                 |}
                 |""".stripMargin) { schema =>
      assertValidate("""1313""".stripMargin)(schema) { result =>
        assertEquals(result.valid, true)
      }
    }
  }

  test("failed oneOf: none") {
    testSchema("""{
                 |"oneOf": [
                 |  { "type": "string" },
                 |  { "type": "boolean" }
                 |]
                 |}
                 |""".stripMargin) { schema =>
      assertValidate("""1313""".stripMargin)(schema) { result =>
        assertEquals(result.valid, false)
        assertEquals(
          result.errors,
          Seq(
            ValidationError(
              NotOneOf(
                0
                // List(
                //   ValidationError(
                //     TypeMismatch("string")
                //   ),
                //   ValidationError(
                //     TypeMismatch("boolean")
                //   )
                // )
              )
            )
          )
        )
      }
    }
  }

  test("failed oneOf: two") {
    testSchema("""{
                 |"oneOf": [
                 |  { "type": "number" },
                 |  { "type": "number" }
                 |]
                 |}
                 |""".stripMargin) { schema =>
      assertValidate("""1313""".stripMargin)(schema) { result =>
        assertEquals(result.valid, false)
        assertEquals(
          result.errors,
          Seq(
            ValidationError(
              NotOneOf(
                2
                // Nil
              )
            )
          )
        )
      }
    }
  }

  test("not") {
    testSchema("""{
                 |"not": { "type": "number" }
                 |}
                 |""".stripMargin) { schema =>
      assertValidate("""true""".stripMargin)(schema) { result =>
        assertEquals(result.valid, true)
      }
    }
  }

  test("failed not") {
    testSchema("""{
                 |"not": { "type": "number" }
                 |}
                 |""".stripMargin) { schema =>
      assertValidate("""1313""".stripMargin)(schema) { result =>
        assertEquals(result.valid, false)
        assertEquals(result.errors, Seq(ValidationError(NotInvalid())))
      }
    }
  }
}
