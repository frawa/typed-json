package frawa.typedjson.parser

import munit._

class ParserTest extends FunSuite {
  implicit val zioParser = new ZioParser();

  test("basic types") {
    assertEquals(Parser.parse("""13"""), Right(NumberValue(13)))
    assertEquals(Parser.parse("""true"""), Right(BoolValue(true)))
    assertEquals(Parser.parse("""null"""), Right(NullValue))
    assertEquals(Parser.parse(""""string""""), Right(StringValue("string")))
  }

  test("array") {
    assertEquals(Parser.parse("""[13]"""), Right(ArrayValue(Seq(NumberValue(13)))))
  }

  test("object") {
    assertEquals(Parser.parse("""{"toto":"titi"}"""), Right(ObjectValue(Map("toto" -> StringValue("titi")))))
  }
}
