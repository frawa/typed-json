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

package frawa.typedjson.parser

import frawa.typedjson.pointer.{OffsetParser, Pointer}
import munit._

class JawnOffsetParserTest extends FunSuite {
  implicit val parser: OffsetParser = new JawnParser()

  test("empty on basic types") {
    assertEquals(parser.pointerAt("""13""")(0), Right(Pointer.empty))
    assertEquals(parser.pointerAt("""13""")(1), Right(Pointer.empty))
    assertEquals(parser.pointerAt("""13""")(13), Right(Pointer.empty))
    assertEquals(parser.pointerAt("""true""")(0), Right(Pointer.empty))
    assertEquals(parser.pointerAt("""false""")(1), Right(Pointer.empty))
    assertEquals(parser.pointerAt("""null""")(13), Right(Pointer.empty))
    assertEquals(parser.pointerAt(""""foo"""")(0), Right(Pointer.empty))
    assertEquals(parser.pointerAt(""""foo"""")(1), Right(Pointer.empty))
    assertEquals(parser.pointerAt(""""foo"""")(13), Right(Pointer.empty))
  }

  test("array") {
    assertEquals(parser.pointerAt("""[13]""")(0), Right(Pointer.empty))
    assertEquals(parser.pointerAt("""[13]""")(1), Right(Pointer.empty / 0))
    assertEquals(parser.pointerAt("""[13]""")(2), Right(Pointer.empty / 0))
    assertEquals(parser.pointerAt("""[13]""")(3), Right(Pointer.empty / 0))
    assertEquals(parser.pointerAt("""[13]""")(4), Right(Pointer.empty / 0))
    assertEquals(parser.pointerAt("""[13]""")(5), Right(Pointer.empty / 0))

    assertEquals(parser.pointerAt("""[13,14,15]""")(3), Right(Pointer.empty / 0))
    assertEquals(parser.pointerAt("""[13,14,15]""")(4), Right(Pointer.empty / 1))
    assertEquals(parser.pointerAt("""[13,14,15]""")(5), Right(Pointer.empty / 1))
    assertEquals(parser.pointerAt("""[13,14,15]""")(6), Right(Pointer.empty / 1))
    assertEquals(parser.pointerAt("""[13,14,15]""")(7), Right(Pointer.empty / 2))
    assertEquals(parser.pointerAt("""[13,14,15]""")(8), Right(Pointer.empty / 2))
    assertEquals(parser.pointerAt("""[13,14,15]""")(9), Right(Pointer.empty / 2))
    assertEquals(parser.pointerAt("""[13,14,15]""")(10), Right(Pointer.empty / 2))
    assertEquals(parser.pointerAt("""[13,14,15]""")(11), Right(Pointer.empty / 2))
  }

  test("nested array") {
    assertEquals(parser.pointerAt("""[1,[2,[3]]]""")(0), Right(Pointer.empty))
    assertEquals(parser.pointerAt("""[1,[2,[3]]]""")(1), Right(Pointer.empty / 0))
    assertEquals(parser.pointerAt("""[1,[2,[3]]]""")(2), Right(Pointer.empty / 0))
    assertEquals(parser.pointerAt("""[1,[2,[3]]]""")(3), Right(Pointer.empty / 0))
    assertEquals(parser.pointerAt("""[1,[2,[3]]]""")(4), Right(Pointer.empty / 1 / 0))
    assertEquals(parser.pointerAt("""[1,[2,[3]]]""")(5), Right(Pointer.empty / 1 / 0))
    assertEquals(parser.pointerAt("""[1,[2,[3]]]""")(6), Right(Pointer.empty / 1 / 0))
    assertEquals(parser.pointerAt("""[1,[2,[3]]]""")(7), Right(Pointer.empty / 1 / 1 / 0))
    assertEquals(parser.pointerAt("""[1,[2,[3]]]""")(8), Right(Pointer.empty / 1 / 1 / 0))
    assertEquals(parser.pointerAt("""[1,[2,[3]]]""")(9), Right(Pointer.empty / 1 / 1))
    assertEquals(parser.pointerAt("""[1,[2,[3]]]""")(10), Right(Pointer.empty / 1))
    assertEquals(parser.pointerAt("""[1,[2,[3]]]""")(11), Right(Pointer.empty / 1))
  }

  test("array with strings") {
    assertEquals(parser.pointerAt("""["a",["b"]]""")(0), Right(Pointer.empty))
    assertEquals(parser.pointerAt("""["a",["b"]]""")(1), Right(Pointer.empty / 0))
    assertEquals(parser.pointerAt("""["a",["b"]]""")(2), Right(Pointer.empty / 0))
    assertEquals(parser.pointerAt("""["a",["b"]]""")(3), Right(Pointer.empty / 0))
    assertEquals(parser.pointerAt("""["a",["b"]]""")(4), Right(Pointer.empty / 0))
    assertEquals(parser.pointerAt("""["a",["b"]]""")(5), Right(Pointer.empty / 0))
    assertEquals(parser.pointerAt("""["a",["b"]]""")(6), Right(Pointer.empty / 1 / 0))
    assertEquals(parser.pointerAt("""["a",["b"]]""")(7), Right(Pointer.empty / 1 / 0))
    assertEquals(parser.pointerAt("""["a",["b"]]""")(8), Right(Pointer.empty / 1 / 0))
    assertEquals(parser.pointerAt("""["a",["b"]]""")(9), Right(Pointer.empty / 1 / 0))
    assertEquals(parser.pointerAt("""["a",["b"]]""")(10), Right(Pointer.empty / 1))
    assertEquals(parser.pointerAt("""["a",["b"]]""")(11), Right(Pointer.empty / 1))
  }

  test("at array end") {
    assertEquals(parser.pointerAt("""[1]""")(2), Right(Pointer.empty / 0))
    assertEquals(parser.pointerAt("""[1,[2]]""")(5), Right(Pointer.empty / 1 / 0))
    assertEquals(parser.pointerAt("""[1,[2]]""")(6), Right(Pointer.empty / 1))
  }

  test("incomplete array".ignore) {
    // TODO recover from broken Json?
    assertEquals(parser.pointerAt("""[1,]""")(3), Right(Pointer.empty / 1))
    assertEquals(parser.pointerAt("""[1,[2,]]""")(6), Right(Pointer.empty / 1 / 1))
  }

  //  test("object") {
//    assertEquals(Parser("""{"toto":"titi"}"""), Right(ObjectValue(Map("toto" -> StringValue("titi")))))
//  }
//
//  test("big number") {
//    assertEquals(
//      Parser("98249283749234923498293171823948729348710298301928331"),
//      Right(NumberValue(BigDecimal("98249283749234923498293171823948729348710298301928331")))
//    )
//  }

}
