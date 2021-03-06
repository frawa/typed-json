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

package frawa.typedjson.jsonSchemaTestSuite

import frawa.typedjson.testutil.TestUtil._

class Draft202012OptionalTest extends JsonSchemaTestSuite {
  import frawa.typedjson.macros.Macros._

//  private val draft202012OptionalFiles: Map[String, Value] =
//    folderJsonContents("./JSON-Schema-Test-Suite/tests/draft2020-12/optional", ".json")
  private val draft202012OptionalFiles: Map[String, String] =
    folderContents("./JSON-Schema-Test-Suite/tests/draft2020-12/optional", ".json")

  // TODO un-ignore 'em
  override val ignore: Set[String] = Set(
//    "bignum.json", // TODO bug/limitation (128bit big decimal?) in Zio parser?, refusing BigDecimal("12345678910111213141516171819202122232425262728293031")
    "ecmascript-regex.json",
    "float-overflow.json",
    "format-assertion.json",
    "refOfUnknownKeyword.json"
  )

  checkFiles(draft202012OptionalFiles)(parseJsonValue)
}
