package frawa.typedjson.schema

import munit._
import frawa.typedjson.parser.ZioParser
import frawa.typedjson.parser.Parser
import frawa.typedjson.parser.{StringValue}
import ValidationResult.{Error}

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

  private def withSchema(text: String)(f: Schema => Unit) {
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
    withSchema("""{"type": "null"}""") { schema =>
      assertValidate("""null""")(schema) { result =>
        assertEquals(result.valid, true)
      }
      assertValidate("""13""")(schema) { result =>
        assertEquals(result.errors, Seq(WithPointer(TypeMismatch(NullSchema))))
      }
    }
  }

  test("boolean") {
    withSchema("""{"type": "boolean"}""") { schema =>
      assertValidate("""true""")(schema) { result =>
        assertEquals(result.valid, true)
      }
      assertValidate("""13""")(schema) { result =>
        assertEquals(result.errors, Seq(WithPointer(TypeMismatch(BooleanSchema))))
      }
    }
  }

  test("boolean false") {
    withSchema("""{"type": "boolean"}""") { schema =>
      assertValidate("""false""")(schema) { result =>
        assertEquals(result.errors, Seq(WithPointer(FalseSchemaReason())))
      }
    }
  }

  test("string") {
    withSchema("""{"type": "string"}""") { schema =>
      assertValidate(""""hello"""")(schema) { result =>
        assertEquals(result.valid, true)
      }
      assertValidate("""13""")(schema) { result =>
        assertEquals(result.errors, Seq(WithPointer(TypeMismatch(StringSchema))))
      }
    }
  }

  test("number") {
    withSchema("""{"type": "number"}""") { schema =>
      assertValidate("""13""")(schema) { result =>
        assertEquals(result.valid, true)
      }
      assertValidate("""null""")(schema) { result =>
        assertEquals(result.errors, Seq(WithPointer(TypeMismatch(NumberSchema))))
      }
    }
  }

  test("array") {
    withSchema("""{"type": "array", "items": { "type": "number"} }""") { schema =>
      assertValidate("""[13]""")(schema) { result =>
        assertEquals(result.valid, true)
      }
      assertValidate("""null""")(schema) { result =>
        assertEquals(result.errors, Seq(WithPointer(TypeMismatch(ArraySchema(NumberSchema)))))
      }
    }
  }

  test("array item") {
    withSchema("""{"type": "array", "items": { "type": "number"} }""") { schema =>
      assertValidate("""[true]""")(schema) { result =>
        assertEquals(result.errors, Seq(WithPointer(TypeMismatch(NumberSchema), Pointer(0))))
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
        assertEquals(result.valid, true)
      }
      assertValidate("""null""")(schema) { result =>
        assertEquals(
          result.errors,
          Seq(
            WithPointer(
              TypeMismatch(
                ObjectSchema(
                  Map(
                    "toto" -> NumberSchema,
                    "titi" -> StringSchema
                  )
                )
              )
            )
          )
        )
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
        assertEquals(result.errors, Seq(WithPointer(TypeMismatch(StringSchema), Pointer.empty / "titi")))
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
        assertEquals(result.errors, Seq(WithPointer(MissingProperties(Map("titi" -> StringSchema)))))
      }
    }
  }

  test("true schema") {
    withSchema("""true""".stripMargin) { schema =>
      assertValidate("""{
                       |"toto": 13
                       |}
                       |""".stripMargin)(schema) { result =>
        assertEquals(result.valid, true)
      }
    }
  }

  test("empty schema") {
    withSchema("""{}""".stripMargin) { schema =>
      assertValidate("""{
                       |"toto": 13
                       |}
                       |""".stripMargin)(schema) { result =>
        assertEquals(result.valid, true)
      }
    }
  }

  test("false schema") {
    withSchema("""false""".stripMargin) { schema =>
      assertValidate("""{
                       |"toto": 13
                       |}
                       |""".stripMargin)(schema) { result =>
        assertEquals(result.valid, false)
        assertEquals(result.errors, Seq(WithPointer(FalseSchemaReason())))
      }
    }
  }

  test("$ref schema") {
    withSchema("""{
                 |"$ref": "#/$defs/toto"
                 |}
                 |""".stripMargin) { schema =>
      assertValidate("""true""".stripMargin)(schema) { result =>
        assertEquals(result.valid, false)
        assertEquals(result.errors, Seq(WithPointer(MissingRef("#/$defs/toto"))))
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
    withSchema("""{
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
    withSchema("""{
                 |"allOf": [
                 |  { "type": "number" },
                 |  { "type": "string" }
                 |]
                 |}
                 |""".stripMargin) { schema =>
      assertValidate("""1313""".stripMargin)(schema) { result =>
        assertEquals(result.valid, false)
        assertEquals(result.errors, Seq(WithPointer(TypeMismatch(StringSchema))))
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
        assertEquals(result.valid, false)
        assertEquals(
          result.errors,
          Seq(
            WithPointer(TypeMismatch(NumberSchema)),
            WithPointer(TypeMismatch(StringSchema))
          )
        )
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
        assertEquals(result.valid, false)
        assertEquals(
          result.errors,
          Seq(
            WithPointer(TypeMismatch(StringSchema)),
            WithPointer(TypeMismatch(BooleanSchema))
          )
        )
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
        assertEquals(result.valid, false)
        assertEquals(
          result.errors,
          Seq(
            WithPointer(NotOneOf(2))
          )
        )
      }
    }
  }

  test("not") {
    withSchema("""{
                 |"not": { "type": "number" }
                 |}
                 |""".stripMargin) { schema =>
      assertValidate("""true""".stripMargin)(schema) { result =>
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
        assertEquals(result.valid, false)
        assertEquals(result.errors, Seq(WithPointer(NotInvalid())))
      }
    }
  }

  test("if/then/else nop") {
    withSchema("""{
                 |"if": { "type": "number" },
                 |"then": { "type": "number" },
                 |"else": { "type": "string" }
                 |}
                 |""".stripMargin) { schema =>
      assertValidate("""1313""".stripMargin)(schema) { result =>
        assertEquals(result.valid, true)
      }
      assertValidate(""""string"""".stripMargin)(schema) { result =>
        assertEquals(result.valid, true)
      }
      assertValidate("""null""".stripMargin)(schema) { result =>
        assertEquals(result.valid, false)
        assertEquals(result.errors, Seq(WithPointer(TypeMismatch(StringSchema))))
      }
    }
  }

  test("null or string") {
    withSchema("""{"type": ["null","string"]}""") { schema =>
      assertValidate("""null""")(schema) { result =>
        assertEquals(result.valid, true)
      }
      assertValidate(""""hello"""")(schema) { result =>
        assertEquals(result.valid, true)
      }
      assertValidate("""13""")(schema) { result =>
        assertEquals(
          result.errors,
          Seq(
            WithPointer(TypeMismatch(NullSchema)),
            WithPointer(TypeMismatch(StringSchema))
          )
        )
      }
    }
  }

  test("enum") {
    withSchema("""{
                 |"type": "string",
                 |"enum": ["foo", "bar"]
                 |}""".stripMargin) { schema =>
      assertValidate(""""foo"""")(schema) { result =>
        assertEquals(result.valid, true)
      }
      assertValidate(""""bar"""")(schema) { result =>
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
      }
    }
  }

  test("const") {
    withSchema("""{
                 |"$id": "testme",
                 |"type": "object",
                 |"properties": { 
                 |  "kind": { "type": "string", "const": "first" }
                 |}
                 |}""".stripMargin) { schema =>
      assertValidate("""{}""".stripMargin)(schema) { result =>
        assertEquals(
          result.errors,
          Seq(
            WithPointer(
              result = MissingProperties(
                properties = Map(
                  "kind" -> SchemaWithValidators(
                    schema = StringSchema,
                    validators = Validators(
                      enum = None,
                      const = Some(
                        value = StringValue(
                          value = "first"
                        )
                      )
                    )
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
      assertValidate("""{"kind":"second"}""")(schema) { result =>
        assertEquals(
          result.errors,
          Seq(
            WithPointer(
              NotInEnum(Seq(StringValue("first"))),
              Pointer(
                segments = List(
                  FieldToken(
                    field = "kind"
                  )
                )
              )
            )
          )
        )
      }
    }
  }
}
