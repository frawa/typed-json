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
    "date-time.json",
    "date.json",
    "idn-hostname.json",
    "uri-template.json"
  )

  override protected val ignoreByExpectation: Map[TestId, Seq[String]] = Map(
    ("duration.json", "validation of duration strings") -> Seq(
      "an invalid duration string",
      "no elements present",
      "no time elements present",
      "no date or time elements present",
      "elements out of order",
      "missing time separator",
      "time element in the date position",
      "weeks cannot be combined with other units",
      "invalid non-ASCII '\u09e8' (a Bengali 2)",
      "element without unit"
    ),
    ("idn-email.json", "validation of an internationalized e-mail addresses") -> Seq(
      "a valid idn e-mail (example@example.test in Hangul)",
      "an invalid idn e-mail address",
      "an invalid e-mail address"
    ),
    ("ipv4.json", "validation of IP addresses") -> Seq(
      "an IP address with too many components",
      "an IP address with out-of-range values",
      "an IP address without 4 components",
      "an IP address as an integer",
      "invalid leading zeroes, as they are treated as octals",
      "invalid non-ASCII '\u09e8' (a Bengali 2)"
    ),
    ("ipv6.json", "validation of IPv6 addresses") -> Seq(
      "mixed format with the ipv4 section as decimal octets",
      "zone id is not a part of ipv6 address",
      "a long valid ipv6",
      "an IPv6 address with out-of-range values",
      "trailing 5 hex symbols is invalid",
      "an IPv6 address with too many components",
      "an IPv6 address containing illegal characters",
      "missing leading octet is invalid",
      "missing trailing octet is invalid",
      "missing leading octet with omitted octets later",
      "two sets of double colons is invalid",
      "mixed format with ipv4 section with octet out of range",
      "mixed format with ipv4 section with a hex octet",
      "triple colons is invalid",
      "insufficient octets without double colons",
      "no colons is invalid",
      "ipv4 is not ipv6",
      "ipv4 segment must have 4 octets",
      "leading whitespace is invalid",
      "trailing whitespace is invalid",
      "netmask is not a part of ipv6 address",
      "a long invalid ipv6, below length limit, first",
      "a long invalid ipv6, below length limit, second",
      "invalid non-ASCII '\u09ea' (a Bengali 4)"
    ),
    ("iri.json", "validation of IRIs") -> Seq(
      "an invalid IRI based on IPv6",
      "an invalid relative IRI Reference",
      "an invalid IRI"
    ),
    ("iri-reference.json", "validation of IRI References") -> Seq(
      "a valid IRI Reference", // TODO fails on JS, passes on JVM
      "an invalid IRI Reference",
      "an invalid IRI fragment"
    ),
    ("uuid.json", "uuid format") -> Seq(
      "wrong length",
      "missing section",
      "bad characters (not hex)",
      "no dashes",
      "too few dashes",
      "too many dashes",
      "dashes in the wrong spot"
    ),
    ("email.json", "validation of e-mail addresses") -> Seq(
      "a quoted string with a space in the local part is valid",
      "an IPv4-address-literal after the @ is valid",
      "an IPv6-address-literal after the @ is valid",
      "an invalid e-mail address",
      "dot before local part is not valid",
      "dot after local part is not valid",
      "two subsequent dots inside local part are not valid",
      "an invalid domain",
      "an invalid IPv4-address-literal"
    ),
    ("time.json", "validation of time strings") -> Seq(
      "a valid time string with leap second, Zulu",
      "valid leap second, zero time-offset",
      "valid leap second, positive time-offset",
      "valid leap second, large positive time-offset",
      "valid leap second, negative time-offset",
      "valid leap second, large negative time-offset",
      "a valid time string with case-insensitive Z",
      "an invalid time string with invalid time numoffset hour",
      "an invalid time string with invalid time numoffset minute",
      "an invalid time string with invalid time with both Z and numoffset",
      "invalid time string with extra leading zeros",
      "invalid time string with no leading zero for single digit",
      "hour, minute, second must be two digits",
      "invalid leap second, Zulu (wrong hour)",
      "invalid leap second, Zulu (wrong minute)",
      "invalid leap second, zero time-offset (wrong hour)",
      "invalid leap second, zero time-offset (wrong minute)",
      "invalid leap second, positive time-offset (wrong hour)",
      "invalid leap second, positive time-offset (wrong minute)",
      "invalid leap second, negative time-offset (wrong hour)",
      "invalid leap second, negative time-offset (wrong minute)",
      "hour, minute in time-offset must be two digits",
      "an invalid time string with invalid hour",
      "an invalid time string with invalid minute",
      "an invalid time string with invalid second",
      "an invalid time string with invalid leap second (wrong hour)",
      "an invalid time string with invalid leap second (wrong minute)",
      "an invalid offset indicator",
      "only RFC3339 not all of ISO 8601 are valid",
      "no time offset",
      "invalid non-ASCII '\u09e8' (a Bengali 2)",
      "offset not starting with plus or minus",
      "contains letters"
    )
  )

  checkFiles(draft202012OptionalFormatFiles)(parseJsonValue)
