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

class Draft202012Test extends JsonSchemaTestSuite {

  import frawa.typedjson.macros.Macros._
  private val draft202012Files: Map[String, String] =
    folderContents("./JSON-Schema-Test-Suite/tests/draft2020-12", ".json")

  // TODO unskip 'em
  override val ignore: Set[String] = Set(
    "content.json" // TODO keywords contentMediaType, contentEncoding, contentSchema
  )

  // TODO unskip 'em
  override val ignoreDescription: Map[String, Set[String]] = Map(
    "vocabulary.json" -> Set(
      "schema that uses custom metaschema with with no validation vocabulary" // TODO support $schema
    )
  )

  override val only: Option[String]            = None
  override val onlyId: Option[String]          = None
  override val onlyDescription: Option[String] = None

  checkFiles(draft202012Files)

}
