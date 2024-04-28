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

package frawa.typedjson.testsuite

import frawa.typedjson.schematestsuite.Draft202012OptionalTestSuite
import frawa.typedjson.testutil.TestUtil.given

import scala.collection.immutable.Seq

class Draft202012OptionalTest extends Draft202012OptionalTestSuite:
  val config = TestConfig(
    ignoreFiles = Seq(
      // "bignum.json", // TODO bug/limitation (128bit big decimal?) in Zio parser?, refusing BigDecimal("12345678910111213141516171819202122232425262728293031")
      "ecmascript-regex.json", // TODO later
      "cross-draft.json"       // TODO later
    ),
    ignoreByExpectationStartsWith = Map(
      (
        "format-assertion.json",
        "schema that uses custom metaschema with format-assertion: false"
      ) -> Seq(
        "format-assertion: false: invalid string"
      ),
      ("dependencies-compatibility.json", "multiple dependents required") -> Seq(
        "missing dependency",
        "missing other dependency",
        "missing both dependencies"
      ),
      ("dependencies-compatibility.json", "single dependency") -> Seq(
        "missing dependency"
      ),
      ("dependencies-compatibility.json", "single schema dependency") -> Seq(
        "wrong type"
      ),
      ("dependencies-compatibility.json", "boolean subschemas") -> Seq(
        "object with property having schema false is invalid",
        "object with both properties is invalid"
      ),
      ("dependencies-compatibility.json", "schema dependencies with escaped characters") -> Seq(
        "quoted quote",
        "quoted tab invalid under dependent schema",
        "CRLF missing dependent"
      ),
      ("dependencies-compatibility.json", "dependencies with escaped characters") -> Seq(
        "CRLF missing dependent",
        "quoted quotes missing dependent"
      )
    )
  )
  suite(config)
