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

package frawa.typedjson.keywords

import frawa.typedjson.util.UriUtil._
import munit.FunSuite

class DynamicScopeTest extends FunSuite:
  import DynamicScope.*

  test("empty") {
    assertEquals(empty.candidates, Seq())
  }

  test("push first pointer segment") {
    val scope = empty.push("toto")
    assertEquals(scope.uris, Seq(uri("#/toto")))
  }

  test("push more pointer segments") {
    val scope = empty
      .push("toto")
      .push("titi")
    assertEquals(
      scope.uris,
      Seq(
        uri("#/toto"),
        uri("#/toto/titi")
      )
    )
  }

  test("push uri") {
    val scope = empty
      .push(uri("toto"))
      .push(uri("titi"))
    assertEquals(
      scope.uris,
      Seq(
        uri("toto"),
        uri("titi")
      )
    )
  }

  test("push mixed") {
    val scope = empty
      .push(uri("toto"))
      .push("foo")
      .push(uri("titi"))
      .push("bar")
    assertEquals(
      scope.uris,
      Seq(
        uri("toto"),
        uri("toto#/foo"),
        uri("titi"),
        uri("titi#/bar")
      )
    )
  }

  test("candidates without fragement") {
    val scope = empty
      .push(uri("toto"))
      .push("foo")
      .push(uri("titi"))
      .push("bar")
    assertEquals(
      scope.candidates,
      Seq(
        uri("toto"),
        uri("titi")
      )
    )
  }
