package frawa.typedjson.schema

import munit._
import frawa.typedjson.parser.ZioParser
import frawa.typedjson.parser.Parser

// see https://json-schema.org/draft/2020-12/json-schema-core.html

class ValidatorTest extends FunSuite {
  implicit val zioParser       = new ZioParser();
  implicit val zioSchemaParser = new ZioSchemaParser();

  test("null") {
    val schema    = SchemaParser("""{"type": "null"}""")
    val validator = schema.flatMap(Validator(_))
    val result    = validator.flatMap(validator => Parser("""null""").map(validator.validate(_)))
    assertEquals(result, Right(None))
    val result2 = validator.flatMap(validator => Parser("""13""").map(validator.validate(_)))
    assertEquals(result2, Right(Some(Seq(Error(TypeMismatch("null"))))))
  }

  test("boolean") {
    val schema    = SchemaParser("""{"type": "boolean"}""")
    val validator = schema.flatMap(Validator(_))
    val result    = validator.flatMap(validator => Parser("""true""").map(validator.validate(_)))
    assertEquals(result, Right(None))
    val result2 = validator.flatMap(validator => Parser("""13""").map(validator.validate(_)))
    assertEquals(result2, Right(Some(Seq(Error(TypeMismatch("boolean"))))))
  }

  test("boolean false") {
    val schema    = SchemaParser("""{"type": "boolean"}""")
    val validator = schema.flatMap(Validator(_))
    val result    = validator.flatMap(validator => Parser("""false""").map(validator.validate(_)))
    assertEquals(result, Right(Some(Seq(Error(FalseSchema())))))
  }

  test("string") {
    val schema    = SchemaParser("""{"type": "string"}""")
    val validator = schema.flatMap(Validator(_))
    val result    = validator.flatMap(validator => Parser(""""hello"""").map(validator.validate(_)))
    assertEquals(result, Right(None))
    val result2 = validator.flatMap(validator => Parser("""13""").map(validator.validate(_)))
    assertEquals(result2, Right(Some(Seq(Error(TypeMismatch("string"))))))
  }

  test("number") {
    val schema    = SchemaParser("""{"type": "number"}""")
    val validator = schema.flatMap(Validator(_))
    val result    = validator.flatMap(validator => Parser("""13""").map(validator.validate(_)))
    assertEquals(result, Right(None))
    val result2 = validator.flatMap(validator => Parser("""null""").map(validator.validate(_)))
    assertEquals(result2, Right(Some(Seq(Error(TypeMismatch("number"))))))
  }

  test("array") {
    val schema    = SchemaParser("""{"type": "array", "items": { "type": "number"} }""")
    val validator = schema.flatMap(Validator(_))
    val result    = validator.flatMap(validator => Parser("""[13]""").map(validator.validate(_)))
    assertEquals(result, Right(None))
    val result2 = validator.flatMap(validator => Parser("""null""").map(validator.validate(_)))
    assertEquals(result2, Right(Some(Seq(Error(TypeMismatch("array"))))))
  }

  test("array item") {
    val schema    = SchemaParser("""{"type": "array", "items": { "type": "number"} }""")
    val validator = schema.flatMap(Validator(_))
    val result    = validator.flatMap(validator => Parser("""[true]""").map(validator.validate(_)))
    assertEquals(result, Right(Some(Seq(Error(TypeMismatch("number"), Pointer(0))))))
  }

  test("object") {
    val schema    = SchemaParser("""{
                                |"type": "object", 
                                |"properties": { 
                                |  "toto": { "type": "number" },
                                |  "titi": { "type": "string" }
                                |} 
                                |}
                                |""".stripMargin)
    val validator = schema.flatMap(Validator(_))
    val result    = validator.flatMap(validator => Parser("""{
                                                         |"toto": 13,
                                                         |"titi": "hello"
                                                         |}
                                                         |""".stripMargin).map(validator.validate(_)))
    assertEquals(result, Right(None))
    val result2 = validator.flatMap(validator => Parser("""null""").map(validator.validate(_)))
    assertEquals(result2, Right(Some(Seq(Error(TypeMismatch("object"))))))
  }

  test("object property type") {
    val schema    = SchemaParser("""{
                                |"type": "object", 
                                |"properties": { 
                                |  "toto": { "type": "number" },
                                |  "titi": { "type": "string" }
                                |} 
                                |}
                                |""".stripMargin)
    val validator = schema.flatMap(Validator(_))
    val result    = validator.flatMap(validator => Parser("""{
                                                         |"toto": 13,
                                                         |"titi": true
                                                         |}
                                                         |""".stripMargin).map(validator.validate(_)))
    assertEquals(result, Right(Some(Seq(Error(TypeMismatch("string"), Pointer.empty / "titi")))))
  }

  test("object unknown property") {
    val schema    = SchemaParser("""{
                                |"type": "object", 
                                |"properties": { 
                                |  "toto": { "type": "number" },
                                |  "titi": { "type": "string" }
                                |} 
                                |}
                                |""".stripMargin)
    val validator = schema.flatMap(Validator(_))
    val result    = validator.flatMap(validator => Parser("""{
                                                         |"gnu": true,
                                                         |"toto": 13,
                                                         |"titi": "foo"
                                                         |}
                                                         |""".stripMargin).map(validator.validate(_)))
    assertEquals(result, Right(Some(Seq(Error(UnexpectedProperty("gnu"))))))
  }

  test("object missing property") {
    val schema    = SchemaParser("""{
                                |"type": "object", 
                                |"properties": { 
                                |  "toto": { "type": "number" },
                                |  "titi": { "type": "string" }
                                |} 
                                |}
                                |""".stripMargin)
    val validator = schema.flatMap(Validator(_))
    val result    = validator.flatMap(validator => Parser("""{
                                                         |"toto": 13
                                                         |}
                                                         |""".stripMargin).map(validator.validate(_)))
    assertEquals(result, Right(Some(Seq(Error(MissingProperty("titi"))))))
  }
}
