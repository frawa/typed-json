package frawa.typedjson.schema

import munit._
import frawa.typedjson.parser.ZioParser
import frawa.typedjson.parser.Parser

// see https://json-schema.org/draft/2020-12/json-schema-core.html

class ValidatorTest extends FunSuite {
  implicit val zioParser    = new ZioParser();
  implicit val schemaParser = SchemaValueDecoder;

  private def testParsedValidator(text: String, parse: String => Either[String, Schema])(f: Validator => Unit) {
    val withValidator = for {
      schema <- parse(text)
      validator = Validator(schema)
    } yield {
      f(validator)
    }
    withValidator.swap
      .map(message => fail("no validator", clues(clue(message))))
      .swap
  }

  private def testValidator(text: String)(f: Validator => Unit) {
    testParsedValidator(text, SchemaParser.schema)(f)
  }

  private def testRootValidator(text: String)(f: Validator => Unit) {
    testParsedValidator(text, SchemaParser.apply)(f)
  }

  private def assertValidate(text: String, validator: Validator)(f: ValidationResult => Unit) = {
    val withParsed = for {
      value <- Parser(text)
      result = Validator.validate(validator)(value)
    } yield {
      f(result)
    }
    withParsed.swap
      .map(message => fail("parsing failed", clues(clue(message))))
      .swap
  }

  test("null") {
    testValidator("""{"type": "null"}""") { validator =>
      val result = Parser("""null""").map(Validator.validate(validator)(_))
      assertValidate("""null""", validator) { result =>
        assertEquals(result.valid, true)
      }
      assertValidate("""13""", validator) { result =>
        assertEquals(result.errors, Seq(ValidationError(TypeMismatch("null"))))
      }
    }
  }

  test("boolean") {
    testValidator("""{"type": "boolean"}""") { validator =>
      assertValidate("""true""", validator) { result =>
        assertEquals(result.valid, true)
      }
      assertValidate("""13""", validator) { result =>
        assertEquals(result.errors, Seq(ValidationError(TypeMismatch("boolean"))))
      }
    }
  }

  test("boolean false") {
    testValidator("""{"type": "boolean"}""") { validator =>
      assertValidate("""false""", validator) { result =>
        assertEquals(result.errors, Seq(ValidationError(FalseSchemaReason())))
      }
    }
  }

  test("string") {
    testValidator("""{"type": "string"}""") { validator =>
      assertValidate(""""hello"""", validator) { result =>
        assertEquals(result.valid, true)
      }
      assertValidate("""13""", validator) { result =>
        assertEquals(result.errors, Seq(ValidationError(TypeMismatch("string"))))
      }
    }
  }

  test("number") {
    testValidator("""{"type": "number"}""") { validator =>
      assertValidate("""13""", validator) { result =>
        assertEquals(result.valid, true)
      }
      assertValidate("""null""", validator) { result =>
        assertEquals(result.errors, Seq(ValidationError(TypeMismatch("number"))))
      }
    }
  }

  test("array") {
    testValidator("""{"type": "array", "items": { "type": "number"} }""") { validator =>
      assertValidate("""[13]""", validator) { result =>
        assertEquals(result.valid, true)
      }
      assertValidate("""null""", validator) { result =>
        assertEquals(result.errors, Seq(ValidationError(TypeMismatch("array"))))
      }
    }
  }

  test("array item") {
    testValidator("""{"type": "array", "items": { "type": "number"} }""") { validator =>
      assertValidate("""[true]""", validator) { result =>
        assertEquals(result.errors, Seq(ValidationError(TypeMismatch("number"), Pointer(0))))
      }
    }
  }

  test("object") {
    testValidator("""{
                    |"type": "object", 
                    |"properties": { 
                    |  "toto": { "type": "number" },
                    |  "titi": { "type": "string" }
                    |} 
                    |}
                    |""".stripMargin) { validator =>
      assertValidate(
        """{
          |"toto": 13,
          |"titi": "hello"
          |}
          |"""".stripMargin,
        validator
      ) { result =>
        assertEquals(result.valid, true)
      }
      assertValidate("""null""", validator) { result =>
        assertEquals(result.errors, Seq(ValidationError(TypeMismatch("object"))))
      }
    }
  }

  test("object property type") {
    testValidator("""{
                    |"type": "object", 
                    |"properties": { 
                    |  "toto": { "type": "number" },
                    |  "titi": { "type": "string" }
                    |} 
                    |}
                    |""".stripMargin) { validator =>
      assertValidate(
        """{
          |"toto": 13,
          |"titi": true
          |}
          |""".stripMargin,
        validator
      ) { result =>
        assertEquals(result.errors, Seq(ValidationError(TypeMismatch("string"), Pointer.empty / "titi")))
      }
    }
  }

  test("object unknown property") {
    testValidator("""{
                    |"type": "object", 
                    |"properties": { 
                    |  "toto": { "type": "number" },
                    |  "titi": { "type": "string" }
                    |} 
                    |}
                    |""".stripMargin) { validator =>
      assertValidate(
        """{
          |"gnu": true,
          |"toto": 13,
          |"titi": "foo"
          |}
          |""".stripMargin,
        validator
      ) { result =>
        assertEquals(result.errors, Seq(ValidationError(UnexpectedProperty("gnu"))))
      }
    }
  }

  test("object missing property") {
    testValidator("""{
                    |"type": "object", 
                    |"properties": { 
                    |  "toto": { "type": "number" },
                    |  "titi": { "type": "string" }
                    |} 
                    |}
                    |""".stripMargin) { validator =>
      assertValidate(
        """{
          |"toto": 13
          |}
          |""".stripMargin,
        validator
      ) { result =>
        assertEquals(result.errors, Seq(ValidationError(MissingProperty("titi"))))
      }
    }
  }

  test("true schema") {
    testValidator("""true""".stripMargin) { validator =>
      assertValidate(
        """{
          |"toto": 13
          |}
          |""".stripMargin,
        validator
      ) { result =>
        assertEquals(result.valid, true)
      }
    }
  }

  test("empty schema") {
    testValidator("""{}""".stripMargin) { validator =>
      assertValidate(
        """{
          |"toto": 13
          |}
          |""".stripMargin,
        validator
      ) { result =>
        assertEquals(result.valid, true)
      }
    }
  }

  test("false schema") {
    testValidator("""false""".stripMargin) { validator =>
      assertValidate(
        """{
          |"toto": 13
          |}
          |""".stripMargin,
        validator
      ) { result =>
        assertEquals(result.valid, false)
        assertEquals(result.errors, Seq(ValidationError(FalseSchemaReason())))
      }
    }
  }

  test("$ref schema") {
    testValidator("""{
                    |"$ref": "#/$defs/toto"
                    |}
                    |""".stripMargin) { validator =>
      assertValidate(
        """true""".stripMargin,
        validator
      ) { result =>
        assertEquals(result.valid, false)
        assertEquals(result.errors, Seq(ValidationError(MissingRef("#/$defs/toto"))))
      }
    }
  }

  test("dereference $ref schema") {
    testRootValidator("""{
                        |"$id": "id13",
                        |"$ref": "#/$defs/toto",
                        |"$defs": {
                        |  "toto": { "type": "number" }
                        |}
                        |}
                        |""".stripMargin) { validator =>
      assertValidate(
        """1313""".stripMargin,
        validator
      ) { result =>
        assertEquals(result.valid, true)
      }
    }
  }
}
