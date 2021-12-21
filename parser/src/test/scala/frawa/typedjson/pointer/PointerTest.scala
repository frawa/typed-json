/*
 * Copyright 2021 Frank Wagner
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package frawa.typedjson.pointer

import frawa.typedjson.parser.Value._
import munit.FunSuite

// see https://datatracker.ietf.org/doc/html/rfc6901

class PointerTest extends FunSuite {
  import Pointer._

  test("empty") {
    assertEquals(empty.toString(), "")
  }

  test("array") {
    assertEquals((empty / 13).toString(), "/13")
  }

  test("field") {
    assertEquals((empty / "toto").toString(), "/toto")
  }

  test("deep field") {
    assertEquals((empty / "$defs" / "gnu").toString, "/$defs/gnu")
  }

  test("field with /") {
    assertEquals((empty / "toto/titi").toString, "/toto~1titi")
  }

  test("field with ~") {
    assertEquals((empty / "toto~titi").toString, "/toto~0titi")
  }

  test("append") {
    val pointer1 = empty / "toto"
    val pointer2 = empty / 13
    assertEquals((pointer1 / pointer2).toString, "/toto/13")
  }

  test("parse") {
    assertEquals(parse("/toto/titi"), empty / "toto" / "titi")
    assertEquals(parse("/$defs/gnu"), empty / "$defs" / "gnu")
    assertEquals(parse("/toto~1titi"), empty / "toto/titi")
    assertEquals(parse("/toto~0titi"), empty / "toto~titi")
  }

  test("get root") {
    val value = NumberValue(13)
    assertEquals(empty(value), Some(value))
  }

  test("get array item") {
    val value = ArrayValue(Seq(NumberValue(13), NumberValue(14)))
    assertEquals((empty / 1)(value), Some(NumberValue(14)))
    assertEquals((empty / 13)(value), None)
  }

  test("object field item") {
    val value = ObjectValue(Map("foo" -> NumberValue(13), "gnu" -> NumberValue(14)))
    assertEquals((empty / "foo")(value), Some(NumberValue(13)))
    assertEquals((empty / "bar")(value), None)
  }

  test("deep object field item") {
    val value = ObjectValue(
      Map("foo" -> ObjectValue(Map("gnu" -> NumberValue(14))), "gnu" -> NumberValue(15))
    )
    assertEquals((empty / "foo" / "gnu")(value), Some(NumberValue(14)))
    assertEquals((empty / "bar")(value), None)
  }

  test("parsing roundtrip") {
    assertEquals(parse(empty.toString), empty)
    val p1 = empty / "foo"
    assertEquals(parse(p1.toString), p1)
    val p2 = empty / "foo" / 13
    assertEquals(parse(p2.toString), p2)

    val s1 = ""
    assertEquals(parse(s1).toString, s1)
    val s2 = "/foo"
    assertEquals(parse(s2).toString, s2)
    val s3 = "/foo/13"
    assertEquals(parse(s3).toString, s3)
  }

  test("get parsed array item") {
    val value = ArrayValue(Seq(NumberValue(13), NumberValue(14)))
    assertEquals(parse("/1")(value), Some(NumberValue(14)))
    assertEquals(parse("/13")(value), None)
  }

  test("parsed object field item") {
    val value = ObjectValue(Map("foo" -> NumberValue(13), "14" -> NumberValue(14)))
    assertEquals(parse("/foo")(value), Some(NumberValue(13)))
    assertEquals(parse("/14")(value), Some(NumberValue(14)))
  }
}
