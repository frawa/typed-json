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

package frawa.typedjson.util

import munit.FunSuite

import SeqUtil._

class SeqUtilTest extends FunSuite:

  test("sequence all rights") {
    assertEquals(
      sequenceAllLefts[String, Integer](Seq(Right(1), Right(2))),
      Right[Seq[String], Seq[Integer]](Seq(1, 2))
    )
  }

  test("sequence only lefts") {
    assertEquals(
      sequenceAllLefts[String, Integer](Seq(Left("foo"), Right(2), Left("bar"))),
      Left(Seq("foo", "bar"))
    )
  }

  test("sequence all mixed lefts") {
    assertEquals(
      sequenceAllLefts[String, Integer](Seq(Right(1), Left("foo"), Right(2), Left("bar"))),
      Left(Seq("foo", "bar"))
    )
  }
