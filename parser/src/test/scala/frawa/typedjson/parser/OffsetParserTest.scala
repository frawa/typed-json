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
  import Offset.*
  import OffsetParser.contextAt
  import OffsetContext.*

  test("contextAt around number") {
    val v = NumberValue(Offset(1, 3), 13)
    assertEquals(contextAt(v)(0), NewValue(Pointer.empty, Offset(0, 0)))
    assertEquals(contextAt(v)(1), InsideValue(Pointer.empty, Offset(1, 3)))
    assertEquals(contextAt(v)(2), InsideValue(Pointer.empty, Offset(1, 3)))
    assertEquals(contextAt(v)(3), NewValue(Pointer.empty, Offset(3, 3)))
    assertEquals(contextAt(v)(13), NewValue(Pointer.empty, Offset(13, 13)))
  }

  test("contextAt around bool") {
    val v = BoolValue(Offset(1, 5), value = true)
    assertEquals(contextAt(v)(0), NewValue(Pointer.empty, Offset(0, 0)))
    assertEquals(contextAt(v)(1), InsideValue(Pointer.empty, Offset(1, 5)))
    assertEquals(contextAt(v)(4), InsideValue(Pointer.empty, Offset(1, 5)))
    assertEquals(contextAt(v)(5), NewValue(Pointer.empty, Offset(5, 5)))
    assertEquals(contextAt(v)(13), NewValue(Pointer.empty, Offset(13, 13)))
  }

  test("contextAt around array") {
    // [13,14]
    val v = ArrayValue(Offset(0, 7), Seq(NumberValue(Offset(1, 3), 13), NumberValue(Offset(4, 6), 14)))
    assertEquals(contextAt(v)(0), NewValue(Pointer.empty, Offset(0, 0)))
    assertEquals(contextAt(v)(1), InsideValue(Pointer.empty / 0, Offset(1, 3)))
    assertEquals(contextAt(v)(2), InsideValue(Pointer.empty / 0, Offset(1, 3)))
    assertEquals(contextAt(v)(3), NewValue(Pointer.empty, Offset(3, 3)))
    assertEquals(contextAt(v)(4), InsideValue(Pointer.empty / 1, Offset(4, 6)))
    assertEquals(contextAt(v)(5), InsideValue(Pointer.empty / 1, Offset(4, 6)))
    assertEquals(contextAt(v)(6), NewValue(Pointer.empty, Offset(6, 6)))
    assertEquals(contextAt(v)(7), NewValue(Pointer.empty, Offset(7, 7)))
    assertEquals(contextAt(v)(13), NewValue(Pointer.empty, Offset(13, 13)))
  }

  test("contextAt around nested array") {
    // [1,[2]]
    val v = ArrayValue(
      Offset(0, 7),
      Seq(NumberValue(Offset(1, 2), 1), ArrayValue(Offset(4, 6), Seq(NumberValue(Offset(4, 5), 2))))
    )
    assertEquals(contextAt(v)(0), NewValue(Pointer.empty, Offset(0, 0)))
    assertEquals(contextAt(v)(1), InsideValue(Pointer.empty / 0, Offset(1, 2)))
    assertEquals(contextAt(v)(2), NewValue(Pointer.empty, Offset(2, 2)))
    assertEquals(contextAt(v)(3), NewValue(Pointer.empty, Offset(3, 3)))
    assertEquals(contextAt(v)(4), InsideValue(Pointer.empty / 1 / 0, Offset(4, 5)))
    assertEquals(contextAt(v)(5), NewValue(Pointer.empty / 1, Offset(5, 5)))
    assertEquals(contextAt(v)(6), NewValue(Pointer.empty, Offset(6, 6)))
    assertEquals(contextAt(v)(7), NewValue(Pointer.empty, Offset(7, 7)))
    assertEquals(contextAt(v)(13), NewValue(Pointer.empty, Offset(13, 13)))
  }

  test("contextAt around object") {
    // {"toto": 13}
    val v = ObjectValue(
      Offset(0, 12),
      Map(StringValue(Offset(1, 7), "toto") -> NumberValue(Offset(9, 11), 13))
    )
    assertEquals(contextAt(v)(0), NewValue(Pointer.empty, Offset(0, 0)))
    assertEquals(contextAt(v)(1), InsideKey(Pointer.empty / "toto", Offset(1, 7)))
    assertEquals(contextAt(v)(2), InsideKey(Pointer.empty / "toto", Offset(1, 7)))
    assertEquals(contextAt(v)(3), InsideKey(Pointer.empty / "toto", Offset(1, 7)))
    assertEquals(contextAt(v)(4), InsideKey(Pointer.empty / "toto", Offset(1, 7)))
    assertEquals(contextAt(v)(5), InsideKey(Pointer.empty / "toto", Offset(1, 7)))
    assertEquals(contextAt(v)(6), InsideKey(Pointer.empty / "toto", Offset(1, 7)))
    assertEquals(contextAt(v)(7), InsideValue(Pointer.empty / "toto", Offset(7, 9)))
    assertEquals(contextAt(v)(8), InsideValue(Pointer.empty / "toto", Offset(8, 9)))
    assertEquals(contextAt(v)(9), InsideValue(Pointer.empty / "toto", Offset(9, 11)))
    assertEquals(contextAt(v)(10), InsideValue(Pointer.empty / "toto", Offset(9, 11)))
    assertEquals(contextAt(v)(11), NewKey(Pointer.empty, Offset(11, 11)))
    assertEquals(contextAt(v)(12), NewValue(Pointer.empty, Offset(12, 12)))
    assertEquals(contextAt(v)(13), NewValue(Pointer.empty, Offset(13, 13)))
  }

  test("contextAt around nexted object") {
    // {"toto": 1, "titi": {"foo": true}}
    val v = ObjectValue(
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
    assertEquals(contextAt(v)(0), NewValue(Pointer.empty, Offset(0, 0)))
    assertEquals(contextAt(v)(1), InsideKey(Pointer.empty / "toto", Offset(1, 7)))
    assertEquals(contextAt(v)(2), InsideKey(Pointer.empty / "toto", Offset(1, 7)))
    assertEquals(contextAt(v)(3), InsideKey(Pointer.empty / "toto", Offset(1, 7)))
    assertEquals(contextAt(v)(4), InsideKey(Pointer.empty / "toto", Offset(1, 7)))
    assertEquals(contextAt(v)(5), InsideKey(Pointer.empty / "toto", Offset(1, 7)))
    assertEquals(contextAt(v)(6), InsideKey(Pointer.empty / "toto", Offset(1, 7)))
    assertEquals(contextAt(v)(7), InsideValue(Pointer.empty / "toto", Offset(7, 9)))
    assertEquals(contextAt(v)(8), InsideValue(Pointer.empty / "toto", Offset(8, 9)))
    assertEquals(contextAt(v)(9), InsideValue(Pointer.empty / "toto", Offset(9, 10)))
    assertEquals(contextAt(v)(10), NewKey(Pointer.empty, Offset(10, 10)))
    assertEquals(contextAt(v)(11), NewKey(Pointer.empty, Offset(11, 11)))
    assertEquals(contextAt(v)(12), InsideKey(Pointer.empty / "titi", Offset(12, 18)))
    assertEquals(contextAt(v)(13), InsideKey(Pointer.empty / "titi", Offset(12, 18)))
    assertEquals(contextAt(v)(14), InsideKey(Pointer.empty / "titi", Offset(12, 18)))
    assertEquals(contextAt(v)(15), InsideKey(Pointer.empty / "titi", Offset(12, 18)))
    assertEquals(contextAt(v)(16), InsideKey(Pointer.empty / "titi", Offset(12, 18)))
    assertEquals(contextAt(v)(17), InsideKey(Pointer.empty / "titi", Offset(12, 18)))
    assertEquals(contextAt(v)(18), InsideValue(Pointer.empty / "titi", Offset(18, 20)))
    assertEquals(contextAt(v)(19), InsideValue(Pointer.empty / "titi", Offset(19, 20)))
    assertEquals(contextAt(v)(20), NewValue(Pointer.empty / "titi", Offset(20, 20)))
    assertEquals(contextAt(v)(21), InsideKey(Pointer.empty / "titi" / "foo", Offset(21, 26)))
    assertEquals(contextAt(v)(22), InsideKey(Pointer.empty / "titi" / "foo", Offset(21, 26)))
    assertEquals(contextAt(v)(23), InsideKey(Pointer.empty / "titi" / "foo", Offset(21, 26)))
    assertEquals(contextAt(v)(24), InsideKey(Pointer.empty / "titi" / "foo", Offset(21, 26)))
    assertEquals(contextAt(v)(25), InsideKey(Pointer.empty / "titi" / "foo", Offset(21, 26)))
    assertEquals(contextAt(v)(26), InsideValue(Pointer.empty / "titi" / "foo", Offset(26, 28)))
    assertEquals(contextAt(v)(27), InsideValue(Pointer.empty / "titi" / "foo", Offset(27, 28)))
    assertEquals(contextAt(v)(28), InsideValue(Pointer.empty / "titi" / "foo", Offset(28, 32)))
    assertEquals(contextAt(v)(29), InsideValue(Pointer.empty / "titi" / "foo", Offset(28, 32)))
    assertEquals(contextAt(v)(30), InsideValue(Pointer.empty / "titi" / "foo", Offset(28, 32)))
    assertEquals(contextAt(v)(31), InsideValue(Pointer.empty / "titi" / "foo", Offset(28, 32)))
    assertEquals(contextAt(v)(32), NewKey(Pointer.empty / "titi", Offset(32, 32)))
    assertEquals(contextAt(v)(33), NewKey(Pointer.empty, Offset(33, 33)))
    assertEquals(contextAt(v)(34), NewValue(Pointer.empty, Offset(34, 34)))
    assertEquals(contextAt(v)(42), NewValue(Pointer.empty, Offset(42, 42)))
  }

  // TODO test on recovered values
  // - arrays
  // - object keys
  // - object values
