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

package frawa.typedjson.eval

import munit.FunSuite

class FormatsTest extends FunSuite:

  import Formats.*

  private def assertFormat(format: String, expected: Boolean)(value: String): Unit =
    val fun      = hasFormat(format)
    val obtained = fun.map(_(value))
    assertEquals(obtained, Some(expected), clue(s"[${format}] ${value}"))

  private def assertHasFormat(format: String)(value: String): Unit =
    assertFormat(format, true)(value)

  private def assertHasNotFormat(format: String)(value: String): Unit =
    assertFormat(format, false)(value)

  test("relative-json-pointer") {
    assertHasFormat("relative-json-pointer")("0/foo/bar")
    assertHasFormat("relative-json-pointer")("0#")
    assertHasNotFormat("relative-json-pointer")("/foo/bar")
    assertHasNotFormat("relative-json-pointer")("-1/foo/bar")
    assertHasNotFormat("relative-json-pointer")("0##")
    assertHasNotFormat("relative-json-pointer")("01/a")
    assertHasFormat("relative-json-pointer")("1")
  }

  test("hostname") {
    assertHasFormat("hostname")("www.example.com")
    assertHasFormat("hostname")("xn--4gbwdl.xn--wgbh1c")
  }
