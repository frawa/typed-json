package frawa.typedjson.schema

import munit.FunSuite
import frawa.typedjson.parser.ZioParser
import frawa.typedjson.parser._
import TestUtil._
import TestSchemas._

class ValidationCheckerTest extends FunSuite {
  implicit val zioParser: ZioParser = new ZioParser()

  private def assertValidate(text: String)(
      schema: SchemaValue,
      lazyResolver: Option[LoadedSchemasResolver.LazyResolver] = None
  )(
      f: Checked[ValidationResult] => Unit
  ) = {
    implicit val l = lazyResolver
    assertChecked(ValidationChecker())(schema, text)(f)
  }

  private def assertErrors(checked: Checked[ValidationResult], expected: Seq[WithPointer[Observation]]): Unit = {
    assertEquals(checked.results.flatMap(_.errors), expected)
  }

  test("null") {
    withSchema(nullSchema) { schema =>
      assertValidate("""null""")(schema) { checked =>
        assertErrors(checked, Seq())
        assertEquals(checked.valid, true)
      }
      assertValidate("""13""")(schema) { checked =>
        assertErrors(checked, Seq(WithPointer(TypeMismatch("null"))))
        assertEquals(checked.valid, false)
      }
    }
  }

  test("boolean") {
    withSchema(boolSchema) { schema =>
      assertValidate("""true""")(schema) { checked =>
        assertErrors(checked, Seq())
        assertEquals(checked.valid, true)
      }
      assertValidate("""13""")(schema) { checked =>
        assertErrors(checked, Seq(WithPointer(TypeMismatch("boolean"))))
        assertEquals(checked.valid, false)
      }
    }
  }

  test("true schema") {
    withSchema(trueSchema) { schema =>
      assertValidate("""null""")(schema) { checked =>
        assertErrors(checked, Seq())
        assertEquals(checked.valid, true)
      }
      assertValidate("""13""")(schema) { checked =>
        assertErrors(checked, Seq())
        assertEquals(checked.valid, true)
      }
      assertValidate("""{}""")(schema) { checked =>
        assertErrors(checked, Seq())
        assertEquals(checked.valid, true)
      }
    }
  }

  test("false schema") {
    withSchema(falseSchema) { schema =>
      assertValidate("""null""")(schema) { checked =>
        assertErrors(
          checked,
          Seq(
            WithPointer(
              result = FalseSchemaReason(),
              pointer = Pointer(
                segments = Nil
              )
            )
          )
        )
        assertEquals(checked.valid, false)
      }
      assertValidate("""13""")(schema) { checked =>
        assertErrors(
          checked,
          Seq(
            WithPointer(
              result = FalseSchemaReason(),
              pointer = Pointer(
                segments = Nil
              )
            )
          )
        )
        assertEquals(checked.valid, false)
      }
      assertValidate("""{}""")(schema) { checked =>
        assertErrors(
          checked,
          Seq(
            WithPointer(
              result = FalseSchemaReason(),
              pointer = Pointer(
                segments = Nil
              )
            )
          )
        )
        assertEquals(checked.valid, false)
      }
    }
  }

  test("not false") {
    withSchema(notFalseSchema) { schema =>
      assertValidate("""null""")(schema) { checked =>
        assertErrors(checked, Seq())
        assertEquals(checked.valid, true)
      }
      assertValidate("""13""")(schema) { checked =>
        assertErrors(checked, Seq())
        assertEquals(checked.valid, true)
      }
      assertValidate("""{}""")(schema) { checked =>
        assertErrors(checked, Seq())
        assertEquals(checked.valid, true)
      }
    }
  }

  test("empty schema") {
    withSchema(emtpySchema) { schema =>
      assertValidate("""null""")(schema) { checked =>
        assertErrors(checked, Seq())
        assertEquals(checked.valid, true)
      }
      assertValidate("""13""")(schema) { checked =>
        assertErrors(checked, Seq())
        assertEquals(checked.valid, true)
      }
      assertValidate("""{}""")(schema) { checked =>
        assertErrors(checked, Seq())
        assertEquals(checked.valid, true)
      }
    }
  }

  test("not empty") {
    withSchema("""{"not": {}}""") { schema =>
      assertValidate("""null""")(schema) { checked =>
        assertErrors(
          checked,
          Seq(
            WithPointer(
              result = NotInvalid(),
              pointer = Pointer(
                segments = Nil
              )
            )
          )
        )
        assertEquals(checked.valid, false)
      }
      assertValidate("""13""")(schema) { checked =>
        assertErrors(
          checked,
          Seq(
            WithPointer(
              result = NotInvalid(),
              pointer = Pointer(
                segments = Nil
              )
            )
          )
        )
        assertEquals(checked.valid, false)
      }
      assertValidate("""{}""")(schema) { checked =>
        assertErrors(
          checked,
          Seq(
            WithPointer(
              result = NotInvalid(),
              pointer = Pointer(
                segments = Nil
              )
            )
          )
        )
        assertEquals(checked.valid, false)
      }
    }
  }

  test("string") {
    withSchema(stringSchema) { schema =>
      assertValidate(""""hello"""")(schema) { checked =>
        assertErrors(checked, Seq())
        assertEquals(checked.valid, true)
      }
      assertValidate("""13""")(schema) { checked =>
        assertErrors(checked, Seq(WithPointer(TypeMismatch("string"))))
        assertEquals(checked.valid, false)
      }
    }
  }

  test("number") {
    withSchema(numberSchema) { schema =>
      assertValidate("""13""")(schema) { checked =>
        assertErrors(checked, Seq())
        assertEquals(checked.valid, true)
      }
      assertValidate("""null""")(schema) { checked =>
        assertErrors(checked, Seq(WithPointer(TypeMismatch("number"))))
        assertEquals(checked.valid, false)
      }
    }
  }

  test("array") {
    withSchema(numberArraySchema) { schema =>
      assertValidate("""[13]""")(schema) { checked =>
        assertErrors(checked, Seq())
        assertEquals(checked.valid, true)
      }
      assertValidate("""null""")(schema) { checked =>
        assertErrors(checked, Seq(WithPointer(TypeMismatch("array"))))
        assertEquals(checked.valid, false)
      }
    }
  }

  test("array item") {
    withSchema(numberArraySchema) { schema =>
      assertValidate("""[true]""")(schema) { checked =>
        assertErrors(checked, Seq(WithPointer(TypeMismatch("number"), Pointer(0))))
        assertEquals(checked.valid, false)
      }
      assertValidate("""[13]""")(schema) { checked =>
        assertErrors(checked, Seq())
        assertEquals(checked.valid, true)
      }
    }
  }

  test("object") {
    withSchema(totoObjectSchema) { schema =>
      assertValidate("""{
                       |"toto": 13,
                       |"titi": "hello"
                       |}
                       |"""".stripMargin)(schema) { checked =>
        assertErrors(checked, Seq())
        assertEquals(checked.valid, true)
      }
      assertValidate("""null""")(schema) { checked =>
        assertErrors(
          checked,
          Seq(
            WithPointer(
              TypeMismatch("object")
            )
          )
        )
        assertEquals(checked.valid, false)
      }
    }
  }

  test("object property type") {
    withSchema(totoObjectSchema) { schema =>
      assertValidate("""{
                       |"toto": 13,
                       |"titi": true
                       |}
                       |""".stripMargin)(schema) { checked =>
        assertErrors(checked, Seq(WithPointer(TypeMismatch("string"), Pointer.empty / "titi")))
        assertEquals(checked.valid, false)
      }
    }
  }

  test("object missing property") {
    withSchema(totoObjectSchema) { schema =>
      assertValidate("""{
                       |"toto": 13
                       |}
                       |""".stripMargin)(schema) { checked =>
        assertErrors(checked, Seq())
        assertEquals(checked.valid, true)
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
                       |""".stripMargin)(schema) { checked =>
        assertErrors(
          checked,
          Seq(
            WithPointer(
              result = MissingRequiredProperties(Seq("titi"))
            )
          )
        )
      }
    }
  }

  test("allOf") {
    withSchema(allOfSchema) { schema =>
      assertValidate("""1313""".stripMargin)(schema) { checked =>
        assertErrors(checked, Seq())
        assertEquals(checked.valid, true)
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
      assertValidate("""1313""".stripMargin)(schema) { checked =>
        assertErrors(checked, Seq(WithPointer(TypeMismatch("string"))))
        assertEquals(checked.valid, false)
      }
    }
  }

  test("anyOf") {
    withSchema(anyOfSchema) { schema =>
      assertValidate("""1313""".stripMargin)(schema) { checked =>
        assertErrors(checked, Seq())
        assertEquals(checked.valid, true)
      }
    }
  }

  test("failed anyOf") {
    withSchema(anyOfSchema) { schema =>
      assertValidate("""true""".stripMargin)(schema) { checked =>
        assertErrors(
          checked,
          Seq(
            WithPointer(TypeMismatch("number")),
            WithPointer(TypeMismatch("string"))
          )
        )
        assertEquals(checked.valid, false)
      }
    }
  }

  test("oneOf") {
    withSchema(oneOfSchema) { schema =>
      assertValidate("""1313""".stripMargin)(schema) { checked =>
        assertErrors(checked, Seq())
        assertEquals(checked.valid, true)
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
      assertValidate("""1313""".stripMargin)(schema) { checked =>
        assertErrors(
          checked,
          Seq(
            WithPointer(TypeMismatch("string")),
            WithPointer(TypeMismatch("boolean"))
          )
        )
        assertEquals(checked.valid, false)
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
      assertValidate("""1313""".stripMargin)(schema) { checked =>
        assertErrors(
          checked,
          Seq(
            WithPointer(NotOneOf(2))
          )
        )
        assertEquals(checked.valid, false)

      }
    }
  }

  test("not") {
    withSchema("""{
                 |"not": { "type": "number" }
                 |}
                 |""".stripMargin) { schema =>
      assertValidate("""true""".stripMargin)(schema) { checked =>
        assertErrors(checked, Seq())
        assertEquals(checked.valid, true)
      }
    }
  }

  test("failed not") {
    withSchema("""{
                 |"not": { "type": "number" }
                 |}
                 |""".stripMargin) { schema =>
      assertValidate("""1313""".stripMargin)(schema) { checked =>
        assertErrors(checked, Seq(WithPointer(NotInvalid())))
        assertEquals(checked.valid, false)
      }
    }
  }

  test("if/then/else") {
    withSchema(ifThenElseSchema) { schema =>
      assertValidate("""1313""".stripMargin)(schema) { checked =>
        assertErrors(checked, Seq())
        assertEquals(checked.valid, true)
      }
      assertValidate(""""string"""".stripMargin)(schema) { checked =>
        assertErrors(checked, Seq())
        assertEquals(checked.valid, true)
      }
      assertValidate("""null""".stripMargin)(schema) { checked =>
        assertErrors(checked, Seq(WithPointer(TypeMismatch("string"))))
        assertEquals(checked.valid, false)
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
      assertValidate("""1313""".stripMargin)(schema) { checked =>
        assertErrors(checked, Seq())
        assertEquals(checked.valid, true)
      }
      assertValidate(""""string"""".stripMargin)(schema) { checked =>
        assertErrors(checked, Seq())
        assertEquals(checked.valid, true)
      }
      assertValidate("""null""".stripMargin)(schema) { checked =>
        assertErrors(checked, Seq(WithPointer(TypeMismatch("string"))))
        assertEquals(checked.valid, false)
      }
    }
  }

  test("if/else") {
    withSchema("""{
                 |"if": { "type": "number" },
                 |"else": { "type": "string" }
                 |}
                 |""".stripMargin) { schema =>
      assertValidate("""1313""".stripMargin)(schema) { checked =>
        assertErrors(checked, Seq())
        assertEquals(checked.valid, true)
      }
      assertValidate(""""string"""".stripMargin)(schema) { checked =>
        assertErrors(checked, Seq())
        assertEquals(checked.valid, true)
      }
      assertValidate("""null""".stripMargin)(schema) { checked =>
        assertErrors(checked, Seq(WithPointer(TypeMismatch("string"))))
        assertEquals(checked.valid, false)
      }
    }
  }

  test("if/then") {
    withSchema("""{
                 |"if": { "type": "number" },
                 |"then": { "type": "number" }
                 |}
                 |""".stripMargin) { schema =>
      assertValidate("""1313""".stripMargin)(schema) { checked =>
        assertErrors(checked, Seq())
        assertEquals(checked.valid, true)
      }
      assertValidate(""""string"""".stripMargin)(schema) { checked =>
        assertErrors(checked, Seq())
        assertEquals(checked.valid, true)
      }
    }
  }

  test("then/else") {
    withSchema("""{
                 |"then": { "type": "number" },
                 |"else": { "type": "string" }
                 |}
                 |""".stripMargin) { schema =>
      assertValidate("""1313""".stripMargin)(schema) { checked =>
        assertErrors(checked, Seq())
        assertEquals(checked.valid, true)
      }
      assertValidate(""""string"""".stripMargin)(schema) { checked =>
        assertErrors(checked, Seq())
        assertEquals(checked.valid, true)
      }
      assertValidate("""null""".stripMargin)(schema) { checked =>
        assertErrors(checked, Seq())
        assertEquals(checked.valid, true)
      }
    }
  }

  test("null or string") {
    withSchema(nullOrStringSchema) { schema =>
      assertValidate("""null""")(schema) { checked =>
        assertErrors(checked, Seq())
        assertEquals(checked.valid, true)
      }
      assertValidate(""""hello"""")(schema) { checked =>
        assertErrors(checked, Seq())
        assertEquals(checked.valid, true)
      }
      assertValidate("""13""")(schema) { checked =>
        assertErrors(
          checked,
          Seq(
            WithPointer(TypeMismatch("null")),
            WithPointer(TypeMismatch("string"))
          )
        )
        assertEquals(checked.valid, false)
      }
    }
  }

  test("enum") {
    withSchema(enumSchema) { schema =>
      assertValidate(""""foo"""")(schema) { checked =>
        assertErrors(checked, Seq())
        assertEquals(checked.valid, true)
      }
      assertValidate(""""bar"""")(schema) { checked =>
        assertErrors(checked, Seq())
        assertEquals(checked.valid, true)
      }
      assertValidate(""""hello"""")(schema) { checked =>
        assertErrors(
          checked,
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
        assertEquals(checked.valid, false)
      }
    }
  }

  test("const") {
    withSchema(constSchema) { schema =>
      assertValidate(""""first"""")(schema) { checked =>
        assertErrors(checked, Seq())
        assertEquals(checked.valid, true)
      }
      assertValidate("""{}""".stripMargin)(schema) { checked =>
        assertErrors(
          checked,
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
      assertValidate(""""second"""")(schema) { checked =>
        assertErrors(
          checked,
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
      assertValidate("""[1313]""".stripMargin)(schema) { checked =>
        assertErrors(checked, Seq())
        assertEquals(checked.valid, true)
      }
      assertValidate(""""string"""".stripMargin)(schema) { checked =>
        assertErrors(
          checked,
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
        assertEquals(checked.valid, false)
      }
    }
  }

  test("$ref in properties") {
    withSchema(refInPropertiesSchema) { schema =>
      assertValidate("""{ "foo": 13 }""".stripMargin)(schema) { checked =>
        assertErrors(checked, Seq())
        assertEquals(checked.valid, true)
      }
      assertValidate("""{ "foo": true }""".stripMargin)(schema) { checked =>
        assertErrors(
          checked,
          Seq(
            WithPointer(
              result = TypeMismatch(
                expected = "number"
              ),
              pointer = Pointer.parse("/foo")
            )
          )
        )
        assertEquals(checked.valid, false)
      }
    }
  }

  test("$ref at root") {
    withSchema(refAtRootSchema) { schema =>
      assertValidate("""{ "foo": 13 }""".stripMargin)(schema) { checked =>
        assertErrors(checked, Seq())
        assertEquals(checked.validation.errors, Seq())
        assertEquals(checked.valid, true)
      }
      assertValidate("""{ "foo": [13] }""".stripMargin)(schema) { checked =>
        assertErrors(checked, Seq())
        assertEquals(checked.validation.errors, Seq())
        assertEquals(checked.valid, true)
      }
      assertValidate("""{ "foo": true }""".stripMargin)(schema) { checked =>
        assertErrors(
          checked,
          Seq(
            WithPointer(
              result = TypeMismatch(
                expected = "number"
              ),
              pointer = Pointer.parse("/foo")
            ),
            WithPointer(
              result = TypeMismatch(
                expected = "array"
              ),
              pointer = Pointer.parse("/foo")
            )
          )
        )
        assertEquals(checked.valid, false)
      }
    }
  }

  test("$ref to validation spec, with two '$ref's") {
    val lazyResolver = Some(SpecMetaSchemas.lazyResolver)

    withSchema(refToValidationSpec) { schema =>
      assertValidate("""{ "$defs": { "foo": { "type": "boolean" } } }""".stripMargin)(schema, lazyResolver) { checked =>
        assertErrors(checked, Seq())
        assertEquals(checked.validation.errors, Seq())
        assertEquals(checked.valid, true)
      }
      assertValidate("""{ "$defs": { "foo": { "type": ["boolean"] } } }""".stripMargin)(schema, lazyResolver) {
        checked =>
          assertErrors(checked, Seq())
          assertEquals(checked.validation.errors, Seq())
          assertEquals(checked.valid, true)
      }
      assertValidate("""{ "$defs": { "foo": { "type": 13 } } }""".stripMargin)(schema, lazyResolver) { checked =>
        assertErrors(
          checked,
          Seq(
            WithPointer(
              result = NotInEnum(
                values = Seq("array", "boolean", "integer", "null", "number", "object", "string").map(StringValue(_))
              ),
              pointer = Pointer.parse("/$defs/foo/type")
            ),
            WithPointer(
              result = TypeMismatch(
                expected = "array"
              ),
              pointer = Pointer.parse("/$defs/foo/type")
            )
          )
        )
        assertEquals(checked.valid, false)
      }
    }
  }

}
