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
import munit.FunSuite

class OffsetParserTest extends FunSuite:
  import OffsetParser.pointerAt
  import Offset._

  test("pointerAt is empty on number") {
    val v = NumberValue(Offset(0, 2), 13)
    assertEquals(pointerAt(v)(0), Pointer.empty)
    assertEquals(pointerAt(v)(1), Pointer.empty)
    assertEquals(pointerAt(v)(13), Pointer.empty)
  }

  test("pointerAt is empty on bool") {
    val v = BoolValue(Offset(0, 4), value = true)
    assertEquals(pointerAt(v)(0), Pointer.empty)
    assertEquals(pointerAt(v)(2), Pointer.empty)
  }

  test("pointerAt is empty on null") {
    val v = NullValue(Offset(0, 4))
    assertEquals(pointerAt(v)(3), Pointer.empty)
  }

  test("pointerAt is empty on string") {
    val v = StringValue(Offset(0, 5), "foo")
    assertEquals(pointerAt(v)(4), Pointer.empty)
  }

  test("pointerAt array") {
    // [13,14]
    val v = ArrayValue(Offset(0, 7), Seq(NumberValue(Offset(1, 3), 13), NumberValue(Offset(4, 6), 14)))
    assertEquals(pointerAt(v)(0), Pointer.empty)
    assertEquals(pointerAt(v)(1), Pointer.empty / 0)
    assertEquals(pointerAt(v)(4), Pointer.empty / 1)
    assertEquals(pointerAt(v)(7), Pointer.empty)
    assertEquals(pointerAt(v)(8), Pointer.empty)
  }

  test("pointerAt nested array") {
    // [1,[2]]
    val v = ArrayValue(
      Offset(0, 7),
      Seq(NumberValue(Offset(1, 2), 1), ArrayValue(Offset(3, 6), Seq(NumberValue(Offset(4, 5), 2))))
    )
    assertEquals(pointerAt(v)(0), Pointer.empty)
    assertEquals(pointerAt(v)(1), Pointer.empty / 0)
    assertEquals(pointerAt(v)(3), Pointer.empty / 1)
    assertEquals(pointerAt(v)(4), Pointer.empty / 1 / 0)
    assertEquals(pointerAt(v)(6), Pointer.empty / 1)
    assertEquals(pointerAt(v)(7), Pointer.empty)
  }

  test("pointerAt object") {
    // {"toto": 13}
    val v = ObjectValue(Offset(0, 13), Map(StringValue(Offset(1, 8), "toto") -> NumberValue(Offset(9, 11), 13)))
    assertEquals(pointerAt(v)(0), Pointer.empty)
    assertEquals(pointerAt(v)(1), (Pointer.empty / "toto").insideKey)
    assertEquals(pointerAt(v)(9), Pointer.empty / "toto")
    assertEquals(pointerAt(v)(12), Pointer.empty)
  }

  test("pointerAt nested object") {
    // {"toto": 1, "titi": {"foo": true}}
    val v = ObjectValue(
      Offset(0, 34),
      Map(
        StringValue(Offset(1, 8), "toto") -> NumberValue(Offset(9, 10), 1),
        StringValue(Offset(12, 19), "titi") -> ObjectValue(
          Offset(20, 33),
          Map(
            StringValue(Offset(21, 27), "foo") -> BoolValue(Offset(29, 32), value = true)
          )
        )
      )
    )
    assertEquals(pointerAt(v)(0), Pointer.empty)
    assertEquals(pointerAt(v)(1), (Pointer.empty / "toto").insideKey)
    assertEquals(pointerAt(v)(9), Pointer.empty / "toto")
    assertEquals(pointerAt(v)(12), (Pointer.empty / "titi").insideKey)
    assertEquals(pointerAt(v)(20), Pointer.empty / "titi")
    assertEquals(pointerAt(v)(21), (Pointer.empty / "titi" / "foo").insideKey)
    assertEquals(pointerAt(v)(29), Pointer.empty / "titi" / "foo")
    assertEquals(pointerAt(v)(33), Pointer.empty / "titi")
    assertEquals(pointerAt(v)(34), Pointer.empty)
  }
