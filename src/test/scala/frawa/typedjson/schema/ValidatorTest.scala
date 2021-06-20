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
    assertEquals(result2, Right(Some(Seq(Error(TypeError("null"))))))
  }

  test("boolean") {
    val schema    = SchemaParser("""{"type": "boolean"}""")
    val validator = schema.flatMap(Validator(_))
    val result    = validator.flatMap(validator => Parser("""true""").map(validator.validate(_)))
    assertEquals(result, Right(None))
    val result2 = validator.flatMap(validator => Parser("""13""").map(validator.validate(_)))
    assertEquals(result2, Right(Some(Seq(Error(TypeError("boolean"))))))
  }

  test("string") {
    val schema    = SchemaParser("""{"type": "string"}""")
    val validator = schema.flatMap(Validator(_))
    val result    = validator.flatMap(validator => Parser(""""hello"""").map(validator.validate(_)))
    assertEquals(result, Right(None))
    val result2 = validator.flatMap(validator => Parser("""13""").map(validator.validate(_)))
    assertEquals(result2, Right(Some(Seq(Error(TypeError("string"))))))
  }

  test("number") {
    val schema    = SchemaParser("""{"type": "number"}""")
    val validator = schema.flatMap(Validator(_))
    val result    = validator.flatMap(validator => Parser("""13""").map(validator.validate(_)))
    assertEquals(result, Right(None))
    val result2 = validator.flatMap(validator => Parser("""null""").map(validator.validate(_)))
    assertEquals(result2, Right(Some(Seq(Error(TypeError("number"))))))
  }

  test("array") {
    val schema    = SchemaParser("""{"type": "array", "items": { "type": "number"} }""")
    val validator = schema.flatMap(Validator(_))
    val result    = validator.flatMap(validator => Parser("""[13]""").map(validator.validate(_)))
    assertEquals(result, Right(None))
    val result2 = validator.flatMap(validator => Parser("""null""").map(validator.validate(_)))
    assertEquals(result2, Right(Some(Seq(Error(TypeError("array"))))))
  }

  test("array item") {
    val schema    = SchemaParser("""{"type": "array", "items": { "type": "number"} }""")
    val validator = schema.flatMap(Validator(_))
    val result    = validator.flatMap(validator => Parser("""[true]""").map(validator.validate(_)))
    assertEquals(result, Right(Some(Seq(Error(TypeError("number"), Pointer(0))))))
  }
}
