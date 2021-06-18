package frawa.typedjson.parser

import munit._

class ParserTest extends FunSuite {
  implicit val zioParser = new ZioParser();

  test("basic types") {
    assertEquals(Parser("""13"""), Right(NumberValue(13)))
    assertEquals(Parser("""true"""), Right(BoolValue(true)))
    assertEquals(Parser("""null"""), Right(NullValue))
    assertEquals(Parser(""""string""""), Right(StringValue("string")))
  }

  test("array") {
    assertEquals(Parser("""[13]"""), Right(ArrayValue(Seq(NumberValue(13)))))
  }

  test("object") {
    assertEquals(Parser("""{"toto":"titi"}"""), Right(ObjectValue(Map("toto" -> StringValue("titi")))))
  }
}
