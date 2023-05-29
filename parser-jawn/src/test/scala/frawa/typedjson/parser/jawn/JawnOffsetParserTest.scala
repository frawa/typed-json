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

package frawa.typedjson.parser.jawn

import frawa.typedjson.parser.{Offset, OffsetParser}
import frawa.typedjson.pointer.Pointer
import munit.*
import frawa.typedjson.parser.OffsetParser.ParseError

class JawnOffsetParserTest extends FunSuite {
  given parser: OffsetParser = new JawnParser()

  import frawa.typedjson.parser.Offset.*

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
      parser.parseWithOffset("""{"toto": 13}"""),
      Right(
        ObjectValue(
          Offset(0, 12),
          Map(StringValue(Offset(1, 7), "toto") -> NumberValue(Offset(9, 11), 13))
        )
      )
    )
    assertEquals(
      parser.parseWithOffset("""{"toto":"titi"}"""),
      Right(
        ObjectValue(
          Offset(0, 15),
          Map(StringValue(Offset(1, 7), "toto") -> StringValue(Offset(8, 14), "titi"))
        )
      )
    )
    assertEquals(
      parser.parseWithOffset("""{"toto":"titi", "foo": 13 }"""),
      Right(
        ObjectValue(
          Offset(0, 27),
          Map(
            StringValue(Offset(1, 7), "toto")  -> StringValue(Offset(8, 14), "titi"),
            StringValue(Offset(16, 21), "foo") -> NumberValue(Offset(23, 25), 13)
          )
        )
      )
    )
  }

  test("object for OffsetParserTest") {
    assertEquals(
      parser.parseWithOffset("""{"toto": 13}"""),
      Right(
        ObjectValue(
          Offset(0, 12),
          Map(StringValue(Offset(1, 7), "toto") -> NumberValue(Offset(9, 11), 13))
        )
      )
    )
    assertEquals(
      parser.parseWithOffset("""{"toto": 1, "titi": {"foo": true}}"""),
      Right(
        ObjectValue(
          Offset(0, 34),
          Map(
            StringValue(Offset(1, 7), "toto") -> NumberValue(Offset(9, 10), 1),
            StringValue(Offset(12, 18), "titi") -> ObjectValue(
              Offset(20, 33),
              Map(
                StringValue(Offset(21, 26), "foo") -> BoolValue(Offset(28, 32), true)
              )
            )
          )
        )
      )
    )
  }

  test("some basic types") {
    assertEquals(
      parser.parseWithOffset("""13"""),
      Right(
        NumberValue(
          Offset(0, 2),
          13
        )
      )
    )
    assertEquals(
      parser.parseWithOffset("""true"""),
      Right(
        BoolValue(
          Offset(0, 4),
          true
        )
      )
    )
    assertEquals(
      parser.parseWithOffset("""null"""),
      Right(
        NullValue(
          Offset(0, 4)
        )
      )
    )
    assertEquals(
      parser.parseWithOffset(""""foo""""),
      Right(
        StringValue(
          Offset(0, 5),
          "foo"
        )
      )
    )
  }

  test("some arrays") {
    assertEquals(
      parser.parseWithOffset("""[13]"""),
      Right(
        ArrayValue(
          Offset(0, 4),
          Seq(NumberValue(Offset(1, 3), 13))
        )
      )
    )
    assertEquals(
      parser.parseWithOffset("""[13,14,15]"""),
      Right(
        ArrayValue(
          Offset(0, 10),
          Seq(NumberValue(Offset(1, 3), 13), NumberValue(Offset(4, 6), 14), NumberValue(Offset(7, 9), 15))
        )
      )
    )
    assertEquals(
      parser.parseWithOffset("""[1,[2,[3]]]"""),
      Right(
        ArrayValue(
          Offset(0, 11),
          Seq(
            NumberValue(Offset(1, 2), 1),
            ArrayValue(
              Offset(3, 10),
              Seq(
                NumberValue(Offset(4, 5), 2),
                ArrayValue(
                  Offset(6, 9),
                  Seq(NumberValue(Offset(7, 8), 3))
                )
              )
            )
          )
        )
      )
    )
  }

  test("some objects") {
    assertEquals(
      parser.parseWithOffset("""{"toto": 13}"""),
      Right(
        ObjectValue(
          Offset(0, 12),
          Map(StringValue(Offset(1, 7), "toto") -> NumberValue(Offset(9, 11), 13))
        )
      )
    )
    assertEquals(
      parser.parseWithOffset("""{"toto": [1,2], "titi": {"foo": true}}"""),
      Right(
        ObjectValue(
          Offset(0, 38),
          Map(
            StringValue(Offset(1, 7), "toto") -> ArrayValue(
              Offset(9, 14),
              Seq(NumberValue(Offset(10, 11), 1), NumberValue(Offset(12, 13), 2))
            ),
            StringValue(Offset(16, 22), "titi") -> ObjectValue(
              Offset(24, 37),
              Map(
                StringValue(Offset(25, 30), "foo") -> BoolValue(
                  Offset(32, 36),
                  true
                )
              )
            )
          )
        )
      )
    )
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
          Offset(0, 10),
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
          Offset(0, 5),
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
          Offset(0, 6),
          Map(
            StringValue(Offset(1, 4), "a") -> NullValue(Offset(5, 5))
          )
        )
      )
    )
  }

  test("a recovered broken object") {
    // like during editing
    val value     = """{"toto":  }"""
    val recovered = recoveredValue(value).get
    assertEquals(
      recovered,
      ObjectValue(
        Offset(0, 11),
        Map(
          StringValue(Offset(1, 7), "toto") -> NullValue(Offset(10, 10))
        )
      )
    )
    // assertEquals(pointerAt(recovered)(9), Pointer.empty / "toto")
    // assertEquals(pointerAt(recovered)(4), (Pointer.empty / "toto"))
  }

  private def offsetAt(json: String)(at: Pointer): Either[OffsetParser.ParseError, Option[Offset]] = {
    parser.parseWithOffset(json).map(OffsetParser.offsetAt(_)(at))
  }

  test("blank") {
    // these crash JS runtime ...
    // fixed using compliant linker options for typed-json-js-export
    val expected = Left(
      value = ParseError(
        offset = 0,
        message = "internal parsing error: exhausted input",
        recoveredValue = None
      )
    )
    assertEquals(
      offsetAt(" ")(Pointer.empty),
      expected
    )
    assertEquals(
      offsetAt("")(Pointer.empty),
      expected
    )
    assertEquals(
      offsetAt("\"")(Pointer.empty),
      expected
    )
    assertEquals(
      offsetAt("   [   ")(Pointer.empty),
      expected
    )
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
    assertEquals(offsetAt("""{}""")(Pointer.empty), Right(Some(Offset(0, 2))))
    assertEquals(offsetAt("""{"toto":13}""")(Pointer.empty), Right(Some(Offset(0, 11))))
    assertEquals(offsetAt("""{"toto":13}""")(Pointer.empty / "toto"), Right(Some(Offset(8, 10))))
    assertEquals(offsetAt("""{"toto":13}""")(Pointer.empty / "missing"), Right(None))
    assertEquals(offsetAt("""{"toto":13}""")(Pointer.empty / 13), Right(None))
    assertEquals(offsetAt("""{"foo": [13,14], "bar": {"gnu": 13}}""")(Pointer.empty), Right(Some(Offset(0, 36))))
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
      Right(Some(Offset(24, 35)))
    )
    assertEquals(
      offsetAt("""{"foo": [13,14], "bar": {"gnu": 13}}""")(Pointer.empty / "bar" / "gnu"),
      Right(Some(Offset(32, 34)))
    )
  }

  test("recover missing value before key") {
    val text = """|{
                  |  "$anchor":      
                  |  "type": "boolean"
                  |}
                  |""".stripMargin
    val recovered = recoveredValue(text).get
    assertEquals(
      recovered,
      ObjectValue(
        Offset(0, 30),
        Map(
          StringValue(Offset(4, 13), "$anchor")
            -> StringValue(Offset(23, 29), "type")
        )
      )
    )
  }

  test("recover missing value at object end") {
    val text = """|{
                  |  "type": "boolean",
                  |  "$anchor":      
                  |}
                  |""".stripMargin
    val recovered = recoveredValue(text).get
    assertEquals(
      recovered,
      ObjectValue(
        Offset(0, 43),
        Map(
          StringValue(Offset(4, 10), "type")
            -> StringValue(Offset(12, 21), "boolean"),
          StringValue(Offset(25, 34), "$anchor")
            -> NullValue(Offset(42, 42))
        )
      )
    )
  }

}
