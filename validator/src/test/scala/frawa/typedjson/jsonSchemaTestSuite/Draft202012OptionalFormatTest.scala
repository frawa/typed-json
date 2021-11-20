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
    "idn-hostname.json",
    "time.json",
    "uri-template.json"
  )

  override protected val ignoreFailMessageByDescription: Map[TestId, Set[String]] = Map(
    ("hostname.json", "validation of host names") -> Set(
      "a valid punycoded IDN hostname",
      "a host name with a component too long",
      "exceeds maximum label length"
    ),
    ("idn-email.json", "validation of an internationalized e-mail addresses") -> Set(
      "a valid idn e-mail (example@example.test in Hangul)"
    ),
    ("ipv6.json", "validation of IPv6 addresses") -> Set(
      "mixed format with the ipv4 section as decimal octets",
      "zone id is not a part of ipv6 address",
      "a long valid ipv6"
    ),
    ("iri.json", "validation of IRIs") -> Set(
      "an invalid IRI based on IPv6"
    ),
    ("iri-reference.json", "validation of IRI References") -> Set(
      "a valid IRI Reference" // TODO fails on JS, passes on JVM
    ),
    ("relative-json-pointer.json", "validation of Relative JSON Pointers (RJP)") -> Set(
      "negative prefix",
      "## is not a valid json-pointer",
      "zero cannot be followed by other digits, plus octothorpe"
    ),
    ("uri.json", "validation of URIs") -> Set(
      "an invalid URI with comma in scheme" // TODO fails on JS, passes on JVM
    ),
    ("uuid.json", "uuid format") -> Set(
      "wrong length"
    )
  )

  checkFiles(draft202012OptionalFormatFiles)
}
