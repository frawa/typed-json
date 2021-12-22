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

  private def pointerAt(json: String)(at: Int): Either[OffsetParser.ParseError, Pointer] = {
    parser.parseWithOffset(json).map(OffsetParser.pointerAt(_)(at))
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
    assertEquals(pointerAt("""{"toto": 13}""")(1), Right((Pointer.empty / "toto").insideKey))
    assertEquals(pointerAt("""{"toto": 13}""")(2), Right((Pointer.empty / "toto").insideKey))
    assertEquals(pointerAt("""{"toto": 13}""")(3), Right((Pointer.empty / "toto").insideKey))
    assertEquals(pointerAt("""{"toto": 13}""")(4), Right((Pointer.empty / "toto").insideKey))
    assertEquals(pointerAt("""{"toto": 13}""")(5), Right((Pointer.empty / "toto").insideKey))
    assertEquals(pointerAt("""{"toto": 13}""")(6), Right((Pointer.empty / "toto").insideKey))
    assertEquals(pointerAt("""{"toto": 13}""")(7), Right((Pointer.empty / "toto").insideKey))
    assertEquals(pointerAt("""{"toto": 13}""")(8), Right(Pointer.empty))
    assertEquals(pointerAt("""{"toto": 13}""")(9), Right(Pointer.empty / "toto"))
    assertEquals(pointerAt("""{"toto": 13}""")(10), Right(Pointer.empty / "toto"))
    assertEquals(pointerAt("""{"toto": 13}""")(11), Right(Pointer.empty / "toto"))
    assertEquals(pointerAt("""{"toto": 13}""")(12), Right(Pointer.empty))
    assertEquals(pointerAt("""{"toto": 13}""")(13), Right(Pointer.empty))
  }

  test("pointerAt nested object") {
    val value = """{"toto": [1,2], "titi": {"foo": true}}"""
    assertEquals(pointerAt(value)(0), Right(Pointer.empty))
    assertEquals(pointerAt(value)(1), Right((Pointer.empty / "toto").insideKey))
    assertEquals(pointerAt(value)(2), Right((Pointer.empty / "toto").insideKey))
    assertEquals(pointerAt(value)(3), Right((Pointer.empty / "toto").insideKey))
    assertEquals(pointerAt(value)(4), Right((Pointer.empty / "toto").insideKey))
    assertEquals(pointerAt(value)(5), Right((Pointer.empty / "toto").insideKey))
    assertEquals(pointerAt(value)(6), Right((Pointer.empty / "toto").insideKey))
    assertEquals(pointerAt(value)(7), Right((Pointer.empty / "toto").insideKey))
    assertEquals(pointerAt(value)(8), Right(Pointer.empty))
    assertEquals(pointerAt(value)(9), Right(Pointer.empty / "toto"))
    assertEquals(pointerAt(value)(10), Right(Pointer.empty / "toto" / 0))
    assertEquals(pointerAt(value)(11), Right(Pointer.empty / "toto" / 0))
    assertEquals(pointerAt(value)(12), Right(Pointer.empty / "toto" / 1))
    assertEquals(pointerAt(value)(13), Right(Pointer.empty / "toto" / 1))
    assertEquals(pointerAt(value)(14), Right(Pointer.empty / "toto"))
    assertEquals(pointerAt(value)(14), Right(Pointer.empty / "toto"))
    assertEquals(pointerAt(value)(15), Right(Pointer.empty))
    assertEquals(pointerAt(value)(16), Right((Pointer.empty / "titi").insideKey))
    assertEquals(pointerAt(value)(17), Right((Pointer.empty / "titi").insideKey))
    assertEquals(pointerAt(value)(18), Right((Pointer.empty / "titi").insideKey))
    assertEquals(pointerAt(value)(19), Right((Pointer.empty / "titi").insideKey))
    assertEquals(pointerAt(value)(20), Right((Pointer.empty / "titi").insideKey))
    assertEquals(pointerAt(value)(21), Right((Pointer.empty / "titi").insideKey))
    assertEquals(pointerAt(value)(22), Right((Pointer.empty / "titi").insideKey))
    assertEquals(pointerAt(value)(23), Right(Pointer.empty))
    assertEquals(pointerAt(value)(24), Right(Pointer.empty / "titi"))
    assertEquals(pointerAt(value)(25), Right((Pointer.empty / "titi" / "foo").insideKey))
    assertEquals(pointerAt(value)(26), Right((Pointer.empty / "titi" / "foo").insideKey))
    assertEquals(pointerAt(value)(27), Right((Pointer.empty / "titi" / "foo").insideKey))
    assertEquals(pointerAt(value)(28), Right((Pointer.empty / "titi" / "foo").insideKey))
    assertEquals(pointerAt(value)(29), Right((Pointer.empty / "titi" / "foo").insideKey))
    assertEquals(pointerAt(value)(30), Right((Pointer.empty / "titi" / "foo").insideKey))
    assertEquals(pointerAt(value)(31), Right(Pointer.empty / "titi"))
    assertEquals(pointerAt(value)(32), Right(Pointer.empty / "titi" / "foo"))
    assertEquals(pointerAt(value)(33), Right(Pointer.empty / "titi" / "foo"))
    assertEquals(pointerAt(value)(34), Right(Pointer.empty / "titi" / "foo"))
    assertEquals(pointerAt(value)(35), Right(Pointer.empty / "titi" / "foo"))
    assertEquals(pointerAt(value)(36), Right(Pointer.empty / "titi" / "foo"))
    assertEquals(pointerAt(value)(37), Right(Pointer.empty))
    assertEquals(pointerAt(value)(38), Right(Pointer.empty))
    assertEquals(pointerAt(value)(39), Right(Pointer.empty))
  }

  private def recoveredValue(json: String): Option[Value] = {
    parser
      .parseWithOffset(json)
      .fold(
        _.recoveredValue,
        { _ =>
          fail("parsing error expected")
          None
        }
      )
  }

  test("recover from broken array") {
    assertEquals(
      recoveredValue("""[1,]"""),
      Some(
        ArrayValue(
          Offset(0, 4),
          Seq(
            NumberValue(Offset(1, 2), 1),
            NullValue(Offset(3, 3))
          )
        )
      )
    )
  }

  test("recover from broken nested array") {
    assertEquals(
      recoveredValue("""[1,[2,]]"""),
      Some(
        ArrayValue(
          Offset(0, 7),
          Seq(
            NumberValue(Offset(1, 2), 1),
            ArrayValue(
              Offset(3, 7),
              Seq(
                NumberValue(Offset(4, 5), 2),
                NullValue(Offset(6, 6))
              )
            )
          )
        )
      )
    )
  }

  test("recover from broken object") {
    assertEquals(
      recoveredValue("""{"a":"b",}"""),
      Some(
        ObjectValue(
          Offset(0, 9),
          Map(
            StringValue(Offset(1, 4), "a") -> StringValue(Offset(5, 8), "b")
          )
        )
      )
    )
  }

  test("recover from broken object key") {
    assertEquals(
      recoveredValue("""{"a"}"""),
      Some(
        ObjectValue(
          Offset(0, 4),
          Map(
            StringValue(Offset(1, 4), "a") -> NullValue(Offset(4, 4))
          )
        )
      )
    )
    assertEquals(
      recoveredValue("""{"a":}"""),
      Some(
        ObjectValue(
          Offset(0, 5),
          Map(
            StringValue(Offset(1, 4), "a") -> NullValue(Offset(5, 5))
          )
        )
      )
    )
  }

  private def offsetAt(json: String)(at: Pointer): Either[OffsetParser.ParseError, Option[Offset]] = {
    parser.parseWithOffset(json).map(OffsetParser.offsetAt(_)(at))
  }

  test("offsetAt is empty on basic types") {
    assertEquals(offsetAt("""13""")(Pointer.empty), Right(Some(Offset(0, 2))))
    assertEquals(offsetAt("""true""")(Pointer.empty), Right(Some(Offset(0, 4))))
    assertEquals(offsetAt("""null""")(Pointer.empty), Right(Some(Offset(0, 4))))
    assertEquals(offsetAt(""""foo"""")(Pointer.empty), Right(Some(Offset(0, 5))))
  }

  test("offsetAt array") {
    assertEquals(offsetAt("""[13]""")(Pointer.empty), Right(Some(Offset(0, 4))))
    assertEquals(offsetAt("""[13]""")(Pointer.empty / 0), Right(Some(Offset(1, 3))))
    assertEquals(offsetAt("""[true]""")(Pointer.empty / 1), Right(None))
    assertEquals(offsetAt("""[1,2]""")(Pointer.empty / 1), Right(Some(Offset(3, 4))))
  }

  test("offsetAt object") {
    assertEquals(offsetAt("""{}""")(Pointer.empty), Right(Some(Offset(0, 1))))
    assertEquals(offsetAt("""{"toto":13}""")(Pointer.empty), Right(Some(Offset(0, 10))))
    assertEquals(offsetAt("""{"toto":13}""")(Pointer.empty / "toto"), Right(Some(Offset(8, 10))))
    assertEquals(offsetAt("""{"toto":13}""")(Pointer.empty / "missing"), Right(None))
    assertEquals(offsetAt("""{"toto":13}""")(Pointer.empty / 13), Right(None))
    assertEquals(offsetAt("""{"foo": [13,14], "bar": {"gnu": 13}}""")(Pointer.empty), Right(Some(Offset(0, 35))))
    assertEquals(
      offsetAt("""{"foo": [13,14], "bar": {"gnu": 13}}""")(Pointer.empty / "foo"),
      Right(Some(Offset(8, 15)))
    )
    assertEquals(
      offsetAt("""{"foo": [13,14], "bar": {"gnu": 13}}""")(Pointer.empty / "foo" / 1),
      Right(Some(Offset(12, 14)))
    )
    assertEquals(
      offsetAt("""{"foo": [13,14], "bar": {"gnu": 13}}""")(Pointer.empty / "bar"),
      Right(Some(Offset(24, 34)))
    )
    assertEquals(
      offsetAt("""{"foo": [13,14], "bar": {"gnu": 13}}""")(Pointer.empty / "bar" / "gnu"),
      Right(Some(Offset(32, 34)))
    )
  }

}
