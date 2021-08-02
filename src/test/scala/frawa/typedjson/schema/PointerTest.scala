package frawa.typedjson.schema

import munit._
import frawa.typedjson.parser.NullValue
import frawa.typedjson.parser.NumberValue
import frawa.typedjson.parser.ArrayValue
import frawa.typedjson.parser.ObjectValue

// see https://datatracker.ietf.org/doc/html/rfc6901

class PointerTest extends FunSuite {

  test("empty") {
    val pointer = Pointer.empty
    assertEquals(pointer.toString, "")
  }

  test("array") {
    assertEquals((Pointer.empty / 13).toString, "/13")
  }

  test("field") {
    assertEquals((Pointer.empty / "toto").toString, "/toto")
  }

  test("field with /") {
    assertEquals((Pointer.empty / "toto/titi").toString, "/toto~1titi")
  }

  test("field with ~") {
    assertEquals((Pointer.empty / "toto~titi").toString, "/toto~0titi")
  }

  test("append") {
    val pointer1 = Pointer.empty / "toto"
    val pointer2 = Pointer.empty / 13
    assertEquals((pointer1 / pointer2).toString, "/toto/13")
  }

  test("parse") {
    assertEquals(Pointer.parse("/toto/titi"), Pointer.empty / "toto" / "titi")
    assertEquals(Pointer.parse("/toto~1titi"), Pointer.empty / "toto/titi")
    assertEquals(Pointer.parse("/toto~0titi"), Pointer.empty / "toto~titi")
  }

  test("get root") {
    val value = NumberValue(13)
    assertEquals(Pointer.empty(value), Some(value))
  }

  test("get array item") {
    val value = ArrayValue(Seq(NumberValue(13), NumberValue(14)))
    assertEquals((Pointer.empty / 1)(value), Some(NumberValue(14)))
    assertEquals((Pointer.empty / 13)(value), None)
  }

  test("object field item") {
    val value = ObjectValue(Map("foo" -> NumberValue(13), "gnu" -> NumberValue(14)))
    assertEquals((Pointer.empty / "foo")(value), Some(NumberValue(13)))
    assertEquals((Pointer.empty / "bar")(value), None)
  }

  test("deep object field item") {
    val value = ObjectValue(
      Map("foo" -> ObjectValue(Map("gnu" -> NumberValue(14))), "gnu" -> NumberValue(15))
    )
    assertEquals((Pointer.empty / "foo" / "gnu")(value), Some(NumberValue(14)))
    assertEquals((Pointer.empty / "bar")(value), None)
  }
}
