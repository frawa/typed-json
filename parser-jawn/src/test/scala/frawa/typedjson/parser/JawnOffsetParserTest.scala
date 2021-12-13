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

import frawa.typedjson.pointer.Pointer
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
      Right(ArrayValue(Offset(0, 4), Seq(NumberValue(Offset(1, 3), 13))))
    )
    assertEquals(
      parser.parseWithOffset("""[13,14]"""),
      Right(
        ArrayValue(
          Offset(0, 7),
          Seq(
            NumberValue(Offset(1, 3), 13),
            NumberValue(Offset(4, 6), 14)
          )
        )
      )
    )
    assertEquals(
      parser.parseWithOffset("""[ 13 , 14 ]"""),
      Right(
        ArrayValue(
          Offset(0, 11),
          Seq(
            NumberValue(Offset(2, 4), 13),
            NumberValue(Offset(7, 9), 14)
          )
        )
      )
    )
  }

  test("nested array") {
    assertEquals(
      parser.parseWithOffset("""[13,[true]]"""),
      Right(
        ArrayValue(
          Offset(0, 11),
          Seq(NumberValue(Offset(1, 3), 13), ArrayValue(Offset(4, 10), Seq(BoolValue(Offset(5, 9), value = true))))
        )
      )
    )
    assertEquals(
      parser.parseWithOffset("""[13,[true,["toto"]]]"""),
      Right(
        ArrayValue(
          Offset(0, 20),
          Seq(
            NumberValue(Offset(1, 3), 13),
            ArrayValue(
              Offset(4, 19),
              Seq(
                BoolValue(Offset(5, 9), value = true),
                ArrayValue(Offset(10, 18), Seq(StringValue(Offset(11, 17), "toto")))
              )
            )
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
          Map(StringValue(Offset(1, 7), "toto") -> StringValue(Offset(8, 14), "titi"))
        )
      )
    )
    assertEquals(
      parser.parseWithOffset("""{"toto":"titi", "foo": 13 }"""),
      Right(
        ObjectValue(
          Offset(0, 26),
          Map(
            StringValue(Offset(1, 7), "toto")  -> StringValue(Offset(8, 14), "titi"),
            StringValue(Offset(16, 21), "foo") -> NumberValue(Offset(23, 25), 13)
          )
        )
      )
    )
  }

  private def pointerAt(json: String)(at: Int): Either[String, Pointer] = {
    parser.parseWithOffset(json).map(parser.pointerAt(_)(at))
  }

  test("pointerAt is empty on basic types") {
    assertEquals(pointerAt("""13""")(0), Right(Pointer.empty))
    assertEquals(pointerAt("""13""")(1), Right(Pointer.empty))
    assertEquals(pointerAt("""13""")(13), Right(Pointer.empty))
    assertEquals(pointerAt("""true""")(0), Right(Pointer.empty))
    assertEquals(pointerAt("""true""")(2), Right(Pointer.empty))
    assertEquals(pointerAt("""null""")(3), Right(Pointer.empty))
    assertEquals(pointerAt(""""foo"""")(4), Right(Pointer.empty))
  }

  test("pointerAt array") {
    assertEquals(pointerAt("""[13]""")(0), Right(Pointer.empty))
    assertEquals(pointerAt("""[13]""")(1), Right(Pointer.empty / 0))
    assertEquals(pointerAt("""[13]""")(2), Right(Pointer.empty / 0))
    assertEquals(pointerAt("""[13]""")(3), Right(Pointer.empty / 0))
    assertEquals(pointerAt("""[13]""")(4), Right(Pointer.empty))

    assertEquals(pointerAt("""[13,14,15]""")(3), Right(Pointer.empty / 0))
    assertEquals(pointerAt("""[13,14,15]""")(4), Right(Pointer.empty / 1))
    assertEquals(pointerAt("""[13,14,15]""")(5), Right(Pointer.empty / 1))
    assertEquals(pointerAt("""[13,14,15]""")(6), Right(Pointer.empty / 1))
    assertEquals(pointerAt("""[13,14,15]""")(7), Right(Pointer.empty / 2))
    assertEquals(pointerAt("""[13,14,15]""")(8), Right(Pointer.empty / 2))
    assertEquals(pointerAt("""[13,14,15]""")(9), Right(Pointer.empty / 2))
    assertEquals(pointerAt("""[13,14,15]""")(10), Right(Pointer.empty))
    assertEquals(pointerAt("""[13,14,15]""")(11), Right(Pointer.empty))
  }

  test("pointerAt nested array") {
    assertEquals(pointerAt("""[1,[2,[3]]]""")(0), Right(Pointer.empty))
    assertEquals(pointerAt("""[1,[2,[3]]]""")(1), Right(Pointer.empty / 0))
    assertEquals(pointerAt("""[1,[2,[3]]]""")(2), Right(Pointer.empty / 0))
    assertEquals(pointerAt("""[1,[2,[3]]]""")(3), Right(Pointer.empty / 1))
    assertEquals(pointerAt("""[1,[2,[3]]]""")(4), Right(Pointer.empty / 1 / 0))
    assertEquals(pointerAt("""[1,[2,[3]]]""")(5), Right(Pointer.empty / 1 / 0))
    assertEquals(pointerAt("""[1,[2,[3]]]""")(6), Right(Pointer.empty / 1 / 1))
    assertEquals(pointerAt("""[1,[2,[3]]]""")(7), Right(Pointer.empty / 1 / 1 / 0))
    assertEquals(pointerAt("""[1,[2,[3]]]""")(8), Right(Pointer.empty / 1 / 1 / 0))
    assertEquals(pointerAt("""[1,[2,[3]]]""")(9), Right(Pointer.empty / 1 / 1))
    assertEquals(pointerAt("""[1,[2,[3]]]""")(10), Right(Pointer.empty / 1))
    assertEquals(pointerAt("""[1,[2,[3]]]""")(11), Right(Pointer.empty))
    assertEquals(pointerAt("""[1,[2,[3]]]""")(12), Right(Pointer.empty))
  }

  test("pointerAt array with strings") {
    assertEquals(pointerAt("""["a",["b"]]""")(0), Right(Pointer.empty))
    assertEquals(pointerAt("""["a",["b"]]""")(1), Right(Pointer.empty / 0))
    assertEquals(pointerAt("""["a",["b"]]""")(2), Right(Pointer.empty / 0))
    assertEquals(pointerAt("""["a",["b"]]""")(3), Right(Pointer.empty / 0))
    assertEquals(pointerAt("""["a",["b"]]""")(4), Right(Pointer.empty / 0))
    assertEquals(pointerAt("""["a",["b"]]""")(5), Right(Pointer.empty / 1))
    assertEquals(pointerAt("""["a",["b"]]""")(6), Right(Pointer.empty / 1 / 0))
    assertEquals(pointerAt("""["a",["b"]]""")(7), Right(Pointer.empty / 1 / 0))
    assertEquals(pointerAt("""["a",["b"]]""")(8), Right(Pointer.empty / 1 / 0))
    assertEquals(pointerAt("""["a",["b"]]""")(9), Right(Pointer.empty / 1 / 0))
    assertEquals(pointerAt("""["a",["b"]]""")(10), Right(Pointer.empty / 1))
    assertEquals(pointerAt("""["a",["b"]]""")(11), Right(Pointer.empty))
    assertEquals(pointerAt("""["a",["b"]]""")(12), Right(Pointer.empty))
  }

  test("pointerAt at array end") {
    assertEquals(pointerAt("""[1]""")(2), Right(Pointer.empty / 0))
    assertEquals(pointerAt("""[1,[2]]""")(5), Right(Pointer.empty / 1 / 0))
    assertEquals(pointerAt("""[1,[2]]""")(6), Right(Pointer.empty / 1))
  }

  test("pointerAt object") {
    assertEquals(pointerAt("""{"toto": 13}""")(0), Right(Pointer.empty))
    assertEquals(pointerAt("""{"toto": 13}""")(1), Right(Pointer.empty))
    assertEquals(pointerAt("""{"toto": 13}""")(2), Right(Pointer.empty))
    assertEquals(pointerAt("""{"toto": 13}""")(3), Right(Pointer.empty))
    assertEquals(pointerAt("""{"toto": 13}""")(3), Right(Pointer.empty))
    assertEquals(pointerAt("""{"toto": 13}""")(4), Right(Pointer.empty))
    assertEquals(pointerAt("""{"toto": 13}""")(5), Right(Pointer.empty))
    assertEquals(pointerAt("""{"toto": 13}""")(6), Right(Pointer.empty))
    assertEquals(pointerAt("""{"toto": 13}""")(7), Right(Pointer.empty))
    assertEquals(pointerAt("""{"toto": 13}""")(8), Right(Pointer.empty))
    assertEquals(pointerAt("""{"toto": 13}""")(9), Right(Pointer.empty / "toto"))
    assertEquals(pointerAt("""{"toto": 13}""")(10), Right(Pointer.empty / "toto"))
    assertEquals(pointerAt("""{"toto": 13}""")(11), Right(Pointer.empty / "toto"))
    assertEquals(pointerAt("""{"toto": 13}""")(12), Right(Pointer.empty))
    assertEquals(pointerAt("""{"toto": 13}""")(13), Right(Pointer.empty))
  }

  test("pointerAt nested object") {
    assertEquals(pointerAt("""{"toto": [1,2], "titi": {"foo": true}}""")(0), Right(Pointer.empty))
    assertEquals(pointerAt("""{"toto": [1,2], "titi": {"foo": true}}""")(1), Right(Pointer.empty))
    assertEquals(pointerAt("""{"toto": [1,2], "titi": {"foo": true}}""")(2), Right(Pointer.empty))
    assertEquals(pointerAt("""{"toto": [1,2], "titi": {"foo": true}}""")(3), Right(Pointer.empty))
    assertEquals(pointerAt("""{"toto": [1,2], "titi": {"foo": true}}""")(4), Right(Pointer.empty))
    assertEquals(pointerAt("""{"toto": [1,2], "titi": {"foo": true}}""")(5), Right(Pointer.empty))
    assertEquals(pointerAt("""{"toto": [1,2], "titi": {"foo": true}}""")(6), Right(Pointer.empty))
    assertEquals(pointerAt("""{"toto": [1,2], "titi": {"foo": true}}""")(7), Right(Pointer.empty))
    assertEquals(pointerAt("""{"toto": [1,2], "titi": {"foo": true}}""")(8), Right(Pointer.empty))
    assertEquals(pointerAt("""{"toto": [1,2], "titi": {"foo": true}}""")(9), Right(Pointer.empty / "toto"))
    assertEquals(pointerAt("""{"toto": [1,2], "titi": {"foo": true}}""")(10), Right(Pointer.empty / "toto" / 0))
    assertEquals(pointerAt("""{"toto": [1,2], "titi": {"foo": true}}""")(11), Right(Pointer.empty / "toto" / 0))
    assertEquals(pointerAt("""{"toto": [1,2], "titi": {"foo": true}}""")(12), Right(Pointer.empty / "toto" / 1))
    assertEquals(pointerAt("""{"toto": [1,2], "titi": {"foo": true}}""")(13), Right(Pointer.empty / "toto" / 1))
    assertEquals(pointerAt("""{"toto": [1,2], "titi": {"foo": true}}""")(14), Right(Pointer.empty / "toto"))
    assertEquals(pointerAt("""{"toto": [1,2], "titi": {"foo": true}}""")(14), Right(Pointer.empty / "toto"))
    assertEquals(pointerAt("""{"toto": [1,2], "titi": {"foo": true}}""")(15), Right(Pointer.empty))
    assertEquals(pointerAt("""{"toto": [1,2], "titi": {"foo": true}}""")(16), Right(Pointer.empty))
    assertEquals(pointerAt("""{"toto": [1,2], "titi": {"foo": true}}""")(17), Right(Pointer.empty))
    assertEquals(pointerAt("""{"toto": [1,2], "titi": {"foo": true}}""")(18), Right(Pointer.empty))
    assertEquals(pointerAt("""{"toto": [1,2], "titi": {"foo": true}}""")(19), Right(Pointer.empty))
    assertEquals(pointerAt("""{"toto": [1,2], "titi": {"foo": true}}""")(20), Right(Pointer.empty))
    assertEquals(pointerAt("""{"toto": [1,2], "titi": {"foo": true}}""")(21), Right(Pointer.empty))
    assertEquals(pointerAt("""{"toto": [1,2], "titi": {"foo": true}}""")(22), Right(Pointer.empty))
    assertEquals(pointerAt("""{"toto": [1,2], "titi": {"foo": true}}""")(23), Right(Pointer.empty))
    assertEquals(pointerAt("""{"toto": [1,2], "titi": {"foo": true}}""")(24), Right(Pointer.empty / "titi"))
    assertEquals(pointerAt("""{"toto": [1,2], "titi": {"foo": true}}""")(25), Right(Pointer.empty / "titi"))
    assertEquals(pointerAt("""{"toto": [1,2], "titi": {"foo": true}}""")(26), Right(Pointer.empty / "titi"))
    assertEquals(pointerAt("""{"toto": [1,2], "titi": {"foo": true}}""")(27), Right(Pointer.empty / "titi"))
    assertEquals(pointerAt("""{"toto": [1,2], "titi": {"foo": true}}""")(28), Right(Pointer.empty / "titi"))
    assertEquals(pointerAt("""{"toto": [1,2], "titi": {"foo": true}}""")(29), Right(Pointer.empty / "titi"))
    assertEquals(pointerAt("""{"toto": [1,2], "titi": {"foo": true}}""")(30), Right(Pointer.empty / "titi"))
    assertEquals(pointerAt("""{"toto": [1,2], "titi": {"foo": true}}""")(31), Right(Pointer.empty / "titi"))
    assertEquals(pointerAt("""{"toto": [1,2], "titi": {"foo": true}}""")(32), Right(Pointer.empty / "titi" / "foo"))
    assertEquals(pointerAt("""{"toto": [1,2], "titi": {"foo": true}}""")(33), Right(Pointer.empty / "titi" / "foo"))
    assertEquals(pointerAt("""{"toto": [1,2], "titi": {"foo": true}}""")(34), Right(Pointer.empty / "titi" / "foo"))
    assertEquals(pointerAt("""{"toto": [1,2], "titi": {"foo": true}}""")(35), Right(Pointer.empty / "titi" / "foo"))
    assertEquals(pointerAt("""{"toto": [1,2], "titi": {"foo": true}}""")(36), Right(Pointer.empty / "titi" / "foo"))
    assertEquals(pointerAt("""{"toto": [1,2], "titi": {"foo": true}}""")(37), Right(Pointer.empty))
    assertEquals(pointerAt("""{"toto": [1,2], "titi": {"foo": true}}""")(38), Right(Pointer.empty))
    assertEquals(pointerAt("""{"toto": [1,2], "titi": {"foo": true}}""")(39), Right(Pointer.empty))
  }

  test("incomplete array".ignore) {
    // TODO recover from broken Json?
    assertEquals(pointerAt("""[1,]""")(3), Right(Pointer.empty / 1))
    assertEquals(pointerAt("""[1,[2,]]""")(6), Right(Pointer.empty / 1 / 1))
  }

}
