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

import frawa.typedjson.schematestsuite.Draft202012OptionalFormatTestSuite
import frawa.typedjson.testutil.TestUtil.given

import scala.collection.immutable.Seq

class Draft202012OptionalFormatTest extends Draft202012OptionalFormatTestSuite:
  val config = TestConfig(
    ignoreFiles = Seq(
      "idn-hostname.json",
      "uri-template.json"
    ),
    ignoreByExpectationStartsWith = Map(
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
        "a valid idn e-mail (example@example.test in Hangul)",
        "an invalid idn e-mail address",
        "an invalid e-mail address"
      ),
      ("ipv4.json", "validation of IP addresses") -> Seq(
        "an IP address with too many components",
        "an IP address with out-of-range values",
        "an IP address without 4 components",
        "an IP address as an integer",
        "an IP address as an integer (decimal)",
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
        "invalid non-ASCII '\u09ea' (a Bengali 4)",
        "invalid non-ASCII '\u09ea' (a Bengali 4) in the IPv4 portion"
      ),
      ("iri.json", "validation of IRIs") -> Seq(
        "an invalid IRI based on IPv6",
        "an invalid relative IRI Reference",
        "an invalid IRI",
        "an invalid IRI though valid IRI reference"
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
      ("uri.json", "validation of URIs") -> Seq(
        "an invalid URI with comma in scheme", // TODO fails on JS, passes on JVM
        "an invalid URI Reference",
        "an invalid URI fragment",
        "an invalid protocol-relative URI Reference",
        "an invalid relative URI Reference",
        "an invalid URI",
        "an invalid URI though valid URI reference",
        "an invalid URI with spaces",
        "an invalid URI with spaces and missing scheme"
      ),
      ("uri-reference.json", "validation of URI References") -> Seq(
        "an invalid URI Reference",
        "an invalid URI fragment"
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
        // TODO java.time does not support leap seconds?!
        "a valid time string with leap second, Zulu",
        "valid leap second, zero time-offset",
        "valid leap second, positive time-offset",
        "valid leap second, large positive time-offset",
        "valid leap second, negative time-offset",
        "valid leap second, large negative time-offset",
        // TODO ScalaJS java.time is different?!
        "an invalid time string with invalid time numoffset hour",
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
        "an invalid time string with invalid time numoffset minute",
        "an invalid time string with invalid time with both Z and numoffset",
        "an invalid offset indicator",
        "only RFC3339 not all of ISO 8601 are valid",
        "no time offset",
        "no time offset with second fraction",
        "invalid non-ASCII '\u09e8' (a Bengali 2)",
        "offset not starting with plus or minus",
        "contains letters"
      ),
      ("date.json", "validation of date strings") -> Seq(
        "a invalid date string with 32 days in January",
        "a invalid date string with 29 days in February (normal)",
        "a invalid date string with 30 days in February (leap)",
        "a invalid date string with 32 days in March",
        "a invalid date string with 31 days in April",
        "a invalid date string with 32 days in May",
        "a invalid date string with 31 days in June",
        "a invalid date string with 32 days in July",
        "a invalid date string with 32 days in August",
        "a invalid date string with 31 days in September",
        "a invalid date string with 32 days in October",
        "a invalid date string with 31 days in November",
        "a invalid date string with 32 days in December",
        "a invalid date string with invalid month",
        "an invalid date string",
        "only RFC3339 not all of ISO 8601 are valid",
        "non-padded month dates are not valid",
        "non-padded day dates are not valid",
        "invalid month",
        "invalid month-day combination",
        "2021 is not a leap year",
        "invalid non-ASCII '\u09ea' (a Bengali 4)",
        "ISO8601 / non-RFC3339: YYYYMMDD without dashes (2023-03-28)",
        "ISO8601 / non-RFC3339: week number implicit day of week (2023-01-02)",
        "ISO8601 / non-RFC3339: week number with day of week (2023-03-28)",
        "ISO8601 / non-RFC3339: week number rollover to next year (2023-01-01)"
      ),
      ("date-time.json", "validation of date-time strings") -> Seq(
        // TODO java.time does not support leap seconds?!
        "a valid date-time with a leap second, UTC",
        "a valid date-time with a leap second, with minus offset",
        "an invalid date-time past leap second, UTC",
        "an invalid date-time with leap second on a wrong minute, UTC",
        "an invalid date-time with leap second on a wrong hour, UTC",
        "an invalid day in date-time string",
        "an invalid offset in date-time string",
        "an invalid closing Z after time-zone offset",
        "an invalid date-time string",
        "only RFC3339 not all of ISO 8601 are valid",
        "invalid non-padded month dates",
        "invalid non-padded day dates",
        "invalid non-ASCII '\u09ea' (a Bengali 4) in date portion",
        "invalid non-ASCII '\u09ea' (a Bengali 4) in time portion"
      ),
      ("duration.json", "validation of duration strings") -> Seq(
        // TODO not supported by java.time?!
        "four years duration",
        "one month duration",
        "two weeks",
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
      ("regex.json", "validation of regular expressions") -> Seq(
        "a regular expression with unclosed parens is invalid"
      ),
      ("json-pointer.json", "validation of JSON-pointers (JSON String Representation)") -> Seq(
        "not a valid JSON-pointer (~ not escaped)",
        "not a valid JSON-pointer (URI Fragment Identifier) #1",
        "not a valid JSON-pointer (URI Fragment Identifier) #2",
        "not a valid JSON-pointer (URI Fragment Identifier) #3",
        "not a valid JSON-pointer (some escaped, but not all) #1",
        "not a valid JSON-pointer (some escaped, but not all) #2",
        "not a valid JSON-pointer (wrong escape character) #1",
        "not a valid JSON-pointer (wrong escape character) #2",
        "not a valid JSON-pointer (multiple characters not escaped)",
        "not a valid JSON-pointer (isn't empty nor starts with /) #1",
        "not a valid JSON-pointer (isn't empty nor starts with /) #2",
        "not a valid JSON-pointer (isn't empty nor starts with /) #3"
      ),
      ("relative-json-pointer.json", "validation of Relative JSON Pointers (RJP)") -> Seq(
        "an invalid RJP that is a valid JSON Pointer",
        "negative prefix",
        "explicit positive prefix",
        "## is not a valid json-pointer",
        "zero cannot be followed by other digits, plus json-pointer",
        "zero cannot be followed by other digits, plus octothorpe",
        "empty string"
      )
    )
  )

  suite(config)
