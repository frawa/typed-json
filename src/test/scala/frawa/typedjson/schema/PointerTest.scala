package frawa.typedjson.schema

import munit._

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
}
