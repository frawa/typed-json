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

import munit._

class JawnOffsetParserTest extends FunSuite {
  implicit val parser: OffsetParser = new JawnParser()

  import Offset._

  test("basic types") {
    assertEquals(parser.parseWithOffset("""13"""), Right(NumberValue(Offset(0, 2), 13)))
    assertEquals(parser.parseWithOffset("""true"""), Right(BoolValue(Offset(0, 4), value = true)))
    assertEquals(parser.parseWithOffset("""null"""), Right(NullValue(Offset(0, 4))))
    assertEquals(
      parser.parseWithOffset(""""string""""),
      Right(StringValue(Offset(0, 8), "string"))
    )
  }

  test("array") {
    assertEquals(
      parser.parseWithOffset("""[13]"""),
      Right(ArrayValue(Offset(0, 3), Seq(NumberValue(Offset(1, 3), 13))))
    )
    assertEquals(
      parser.parseWithOffset("""[ 13 , 14 ]"""),
      Right(
        ArrayValue(
          Offset(0, 10),
          Seq(
            NumberValue(Offset(2, 4), 13),
            NumberValue(Offset(7, 9), 14)
          )
        )
      )
    )
  }

  test("object") {
    assertEquals(
      parser.parseWithOffset("""{"toto":"titi"}"""),
      Right(
        ObjectValue(
          Offset(0, 14),
          Map("toto" -> StringValue(Offset(8, 14), "titi"))
        )
      )
    )
    assertEquals(
      parser.parseWithOffset("""{"toto":"titi", "foo": 13 }"""),
      Right(
        ObjectValue(
          Offset(0, 26),
          Map(
            "toto" -> StringValue(Offset(8, 14), "titi"),
            "foo"  -> NumberValue(Offset(23, 25), 13)
          )
        )
      )
    )
  }

  /*
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

  test("object") {
    assertEquals(parser.pointerAt("""{"a":"b"}""")(0), Right(Pointer.empty))
    assertEquals(parser.pointerAt("""{"a":"b"}""")(3), Right(Pointer.empty / "a"))
    assertEquals(parser.pointerAt("""{"a":"b"}""")(6), Right(Pointer.empty / "a"))
  }
   */
}
