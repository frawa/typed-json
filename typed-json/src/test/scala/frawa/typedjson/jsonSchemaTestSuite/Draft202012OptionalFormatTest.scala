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

import frawa.inlinefiles.InlineFiles.*
import frawa.typedjson.parser.*
import frawa.typedjson.testutil.TestUtil.{*, given}

class Draft202012OptionalFormatTest extends JsonSchemaTestSuite:

  private val draft202012OptionalFormatFiles = draft202012.folder("optional/format").files()

  override val useFormatAssertion = true

  // TODO un-ignore 'em
  override val ignoreFiles: Seq[String] = Seq(
    "idn-hostname.json",
    "uri-template.json"
  )

  // TODO un-ignore 'em
  override protected val ignoreByExpectation: Map[TestId, Seq[String]] = Map(
    ("hostname.json", "validation of host names") -> Seq(
      "a host name containing illegal characters",
      "a host name with a component too long",
      "contains underscore",
      "exceeds maximum label length",
      // TODO fails on JS, passes on JVM
      "a host name starting with an illegal character",
      "starts with hyphen",
      "ends with hyphen",
      "starts with underscore",
      "ends with underscore"
    ),
    ("idn-email.json", "validation of an internationalized e-mail addresses") -> Seq(
      "a valid idn e-mail (example@example.test in Hangul)"
    ),
    ("ipv6.json", "validation of IPv6 addresses") -> Seq(
      "mixed format with the ipv4 section as decimal octets",
      "zone id is not a part of ipv6 address",
      "a long valid ipv6"
    ),
    ("iri.json", "validation of IRIs") -> Seq(
      "an invalid IRI based on IPv6"
    ),
    ("iri-reference.json", "validation of IRI References") -> Seq(
      "a valid IRI Reference" // TODO fails on JS, passes on JVM
    ),
    ("uuid.json", "uuid format") -> Seq(
      "wrong length"
    ),
    ("uri.json", "validation of URIs") -> Seq(
      "an invalid URI with comma in scheme" // TODO fails on JS, passes on JVM
    ),
    ("email.json", "validation of e-mail addresses") -> Seq(
      "a quoted string with a space in the local part is valid",
      "an IPv4-address-literal after the @ is valid",
      "an IPv6-address-literal after the @ is valid"
    ),
    ("time.json", "validation of time strings") -> Seq(
      // TODO java.time does not support leap seconds?!
      "a valid time string with leap second, Zulu",
      "valid leap second, zero time-offset",
      "valid leap second, positive time-offset",
      "valid leap second, large positive time-offset",
      "valid leap second, negative time-offset",
      "valid leap second, large negative time-offset",
      // TODO ScalaJS java.time is different?!
      "an invalid time string with invalid time numoffset hour"
    ),
    ("date-time.json", "validation of date-time strings") -> Seq(
      // TODO java.time does not support leap seconds?!
      "a valid date-time with a leap second, UTC",
      "a valid date-time with a leap second, with minus offset"
    ),
    ("duration.json", "validation of duration strings") -> Seq(
      // TODO not supported by java.time?!
      "four years duration",
      "one month duration",
      "two weeks"
    )
  )

  checkFiles(draft202012OptionalFormatFiles)(parseJsonValue)
