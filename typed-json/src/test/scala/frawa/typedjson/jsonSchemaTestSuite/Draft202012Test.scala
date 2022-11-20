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

import frawa.typedjson.testutil.TestUtil.{*, given}
import frawa.typedjson.macros.Macros

import frawa.inlinefiles.InlineFiles.*

class Draft202012Test extends JsonSchemaTestSuite:
  private val draft202012Files = draft202012.files()

  // override protected val onlyDescription: Option[String] = Some("unevaluatedItems with nested items")

  // TODO un-ignore 'em
  override val ignore: Set[String] = Set(
    "content.json" // TODO keywords contentMediaType, contentEncoding, contentSchema
  )

  // TODO un-ignore 'em
  override val ignoreDescriptionByFile: Map[String, Set[String]] = Map(
    "id.json" -> Set(
      "Invalid use of fragments in location-independent $id" // TODO format "uri-reference"
    )
  )

  checkFiles(draft202012Files)(parseJsonValue)
