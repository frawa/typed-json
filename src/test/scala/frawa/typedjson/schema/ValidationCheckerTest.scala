package frawa.typedjson.schema

import munit.FunSuite
import frawa.typedjson.parser.ZioParser
import frawa.typedjson.parser._
import TestUtil._
import TestSchemas._
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

  test("null") {
    withSchema(nullSchema) { schema =>
      assertValidate("""null""")(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate("""13""")(schema) { result =>
        assertEquals(result.errors, Seq(WithPointer(TypeMismatch("null"))))
        assertEquals(result.valid, false)
      }
    }
  }

  test("boolean") {
    withSchema(boolSchema) { schema =>
      assertValidate("""true""")(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate("""13""")(schema) { result =>
        assertEquals(result.errors, Seq(WithPointer(TypeMismatch("boolean"))))
        assertEquals(result.valid, false)
      }
    }
  }

  test("true schema") {
    withSchema(trueSchema) { schema =>
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
    withSchema(falseSchema) { schema =>
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
    withSchema(notFalseSchema) { schema =>
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
    withSchema(emtpySchema) { schema =>
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
    withSchema(stringSchema) { schema =>
      assertValidate(""""hello"""")(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate("""13""")(schema) { result =>
        assertEquals(result.errors, Seq(WithPointer(TypeMismatch("string"))))
        assertEquals(result.valid, false)
      }
    }
  }

  test("number") {
    withSchema(numberSchema) { schema =>
      assertValidate("""13""")(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate("""null""")(schema) { result =>
        assertEquals(result.errors, Seq(WithPointer(TypeMismatch("number"))))
        assertEquals(result.valid, false)
      }
    }
  }

  test("array") {
    withSchema(numberArraySchema) { schema =>
      assertValidate("""[13]""")(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate("""null""")(schema) { result =>
        assertEquals(result.errors, Seq(WithPointer(TypeMismatch("array"))))
        assertEquals(result.valid, false)
      }
    }
  }

  test("array item") {
    withSchema(numberArraySchema) { schema =>
      assertValidate("""[true]""")(schema) { result =>
        assertEquals(result.errors, Seq(WithPointer(TypeMismatch("number"), Pointer(0))))
        assertEquals(result.valid, false)
      }
      assertValidate("""[13]""")(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
    }
  }

  test("object") {
    withSchema(totoObjectSchema) { schema =>
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
              TypeMismatch("object")
            )
          )
        )
        assertEquals(result.valid, false)
      }
    }
  }

  test("object property type") {
    withSchema(totoObjectSchema) { schema =>
      assertValidate("""{
                       |"toto": 13,
                       |"titi": true
                       |}
                       |""".stripMargin)(schema) { result =>
        assertEquals(result.errors, Seq(WithPointer(TypeMismatch("string"), Pointer.empty / "titi")))
        assertEquals(result.valid, false)
      }
    }
  }

  test("object unknown property") {
    withSchema(totoObjectSchema) { schema =>
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
    withSchema(totoObjectSchema) { schema =>
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
              result = MissingProperties(Seq("titi"))
            )
          )
        )
      }
    }
  }

  test("allOf") {
    withSchema(allOfSchema) { schema =>
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
        assertEquals(result.errors, Seq(WithPointer(TypeMismatch("string"))))
        assertEquals(result.valid, false)
      }
    }
  }

  test("anyOf") {
    withSchema(anyOfSchema) { schema =>
      assertValidate("""1313""".stripMargin)(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
    }
  }

  test("failed anyOf") {
    withSchema(anyOfSchema) { schema =>
      assertValidate("""true""".stripMargin)(schema) { result =>
        assertEquals(
          result.errors,
          Seq(
            WithPointer(TypeMismatch("number")),
            WithPointer(TypeMismatch("string"))
          )
        )
        assertEquals(result.valid, false)
      }
    }
  }

  test("oneOf") {
    withSchema(oneOfSchema) { schema =>
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
            WithPointer(TypeMismatch("string")),
            WithPointer(TypeMismatch("boolean"))
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
    withSchema(ifThenElseSchema) { schema =>
      assertValidate("""1313""".stripMargin)(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate(""""string"""".stripMargin)(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate("""null""".stripMargin)(schema) { result =>
        assertEquals(result.errors, Seq(WithPointer(TypeMismatch("string"))))
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
        assertEquals(result.errors, Seq(WithPointer(TypeMismatch("string"))))
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
        assertEquals(result.errors, Seq(WithPointer(TypeMismatch("string"))))
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
    withSchema(nullOrStringSchema) { schema =>
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
            WithPointer(TypeMismatch("null")),
            WithPointer(TypeMismatch("string"))
          )
        )
        assertEquals(result.valid, false)
      }
    }
  }

  test("enum") {
    withSchema(enumSchema) { schema =>
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
    withSchema(constSchema) { schema =>
      assertValidate(""""first"""")(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate("""{}""".stripMargin)(schema) { result =>
        assertEquals(
          result.errors,
          Seq(
            WithPointer(
              result = TypeMismatch(
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

  test("$id/$ref/$def") {
    withSchema(idRefDefsSchema) { schema =>
      assertValidate("""[1313]""".stripMargin)(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate(""""string"""".stripMargin)(schema) { result =>
        assertEquals(
          result.errors,
          Seq(
            WithPointer(
              result = TypeMismatch(
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
