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

class Draft202012OptionalFormatTest extends JsonSchemaTestSuite {

  import frawa.typedjson.macros.Macros._

  private val draft202012OptionalFormatFiles: Map[String, String] =
    folderContents("./JSON-Schema-Test-Suite/tests/draft2020-12/optional/format", ".json")

  // TODO un-ignore 'em
  override val ignore: Set[String] = Set(
    "date-time.json",
    "date.json",
    "hostname.json",
    "idn-email.json",
    "idn-hostname.json",
    "ipv4.json",
    "ipv6.json",
    "iri.json",
    "json-pointer.json",
    "relative-json-pointer.json",
    "time.json",
    "uri-template.json",
    "uuid.json"
  )

  checkFiles(draft202012OptionalFormatFiles)
}
