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

package frawa.typedjson.formats

import frawa.typedjson.pointer.Pointer

import java.net.URI
import java.util.UUID
import java.util.regex.Pattern
import scala.util.Try
import frawa.typesjson.formats.Platform
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import java.time.Duration
import java.time.OffsetTime
import java.time.OffsetDateTime

object Formats:
  def hasFormat(format: String): Option[String => Boolean] =
    format match
      case "regex" =>
        Some(v => Try(Pattern.compile(v)).isSuccess)
      case "email" =>
        // see https://emailregex.com/
        val regex =
          """(?:[a-z\\d!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z\\d!#$%&'*+/=?^_`{|}~-]+)*|"(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21\x23-\x5b\x5d-\x7f]|\\[\x01-\x09\x0b\x0c\x0e-\x7f])*")@(?:(?:[a-z\\d](?:[a-z\\d-]*[a-z\\d])?\.)+[a-z\\d](?:[a-z\\d-]*[a-z\\d])?|\[(?:(?:25[0-5]|2[0-4][\\d]|[01]?[\\d][\\d]?)\.){3}(?:25[0-5]|2[0-4][\\d]|[01]?[\\d][\\d]?|[a-z\\d-]*[a-z\\d]:(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21-\x5a\x53-\x7f]|\\[\x01-\x09\x0b\x0c\x0e-\x7f])+)\])""".r
        Some(v => regex.matches(v))
      case "idn-email" =>
        // see https://stackoverflow.com/questions/13992403/regex-validation-of-email-addresses-according-to-rfc5321-rfc5322
        val regex =
          """([!#-'*+/-9=?A-Z^-~-]+(\.[!#-'*+/-9=?A-Z^-~-]+)*|"([ ]!#-[^-~ \t]|(\\[\t -~]))+")@([!#-'*+/-9=?A-Z^-~-]+(\.[!#-'*+/-9=?A-Z^-~-]+)*|\[[\t -Z^-~]*])""".r
        Some(v => regex.matches(v))
      case "hostname" =>
        // https://www.rfc-editor.org/rfc/rfc1123.txt
        // https://www.rfc-editor.org/rfc/rfc5891.txt
        // see https://stackoverflow.com/questions/106179/regular-expression-to-match-dns-hostname-or-ip-address
        // val pattern = raw"^[a-zA-Z0-9\p{L}][a-zA-Z0-9\p{L}-\.]{1,61}[a-zA-Z0-9\p{L}]\.[a-zA-Z\p{L}]{2,}$$/gmu".r
        // val pattern = raw"^(?:[\p{L}\p{N}][\p{L}\p{N}-_]*.)+[\p{L}\p{N}]{2,}$$".r
        // Some(v => pattern.matches(v))
        Some(Platform.verifyHostname)
      case "idn-hostname" =>
        val pattern = raw"^[a-zA-Z0-9\p{L}][a-zA-Z0-9\p{L}-\.]{1,61}[a-zA-Z0-9\p{L}]\.[a-zA-Z\p{L}]{2,}$$/gmu".r
        Some(v => pattern.matches(v))
      case "ipv4" =>
        // see https://stackoverflow.com/questions/106179/regular-expression-to-match-dns-hostname-or-ip-address
        val regex =
          """^(([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])\.){3}([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])$""".r
        Some(v => regex.matches(v))
      case "ipv6" =>
        // see https://stackoverflow.com/questions/53497/regular-expression-that-matches-valid-ipv6-addresses
        val regex =
          """(([0-9a-fA-F]{1,4}:){7,7}[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,7}:|([0-9a-fA-F]{1,4}:){1,6}:[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,5}(:[0-9a-fA-F]{1,4}){1,2}|([0-9a-fA-F]{1,4}:){1,4}(:[0-9a-fA-F]{1,4}){1,3}|([0-9a-fA-F]{1,4}:){1,3}(:[0-9a-fA-F]{1,4}){1,4}|([0-9a-fA-F]{1,4}:){1,2}(:[0-9a-fA-F]{1,4}){1,5}|[0-9a-fA-F]{1,4}:((:[0-9a-fA-F]{1,4}){1,6})|:((:[0-9a-fA-F]{1,4}){1,7}|:)|fe80:(:[0-9a-fA-F]{0,4}){0,4}%[0-9a-zA-Z]{1,}|::(ffff(:0{1,4}){0,1}:){0,1}((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])|([0-9a-fA-F]{1,4}:){1,4}:((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]))""".r
        Some(v => regex.matches(v))
      case "uri" =>
        Some(v => Try(new URI(v)).map(_.isAbsolute).getOrElse(false))
      case "uri-reference" =>
        // TODO reference?
        Some(v => Try(new URI(v)).isSuccess)
      case "uri-template" =>
        // see https://datatracker.ietf.org/doc/html/rfc6570
        // TODO template?
        // TODO may use for iri?
        Some(v => Try(new URI(v)).isSuccess)
      case "iri" =>
        // see https://datatracker.ietf.org/doc/html/rfc3987
        // TODO iri
        Some(v => Try(new URI(v)).map(_.isAbsolute).getOrElse(false))
      case "iri-reference" =>
        // see https://datatracker.ietf.org/doc/html/rfc3987
        // TODO iri
        // TODO reference?
        Some(v => Try(new URI(v)).isSuccess)
      case "uuid" =>
        // see https://datatracker.ietf.org/doc/html/rfc4122
        Some(v => Try(UUID.fromString(v)).isSuccess)
      case "json-pointer" =>
        // https://www.rfc-editor.org/rfc/rfc6901.txt
        Some(v => isJsonPointer(v))
      case "relative-json-pointer" =>
        // https://datatracker.ietf.org/doc/html/draft-handrews-relative-json-pointer-01
        val pattern = "(0|[1-9][0-9]*)(#|.*)".r
        Some(v =>
          v match {
            case pattern(nr, "#")     => true
            case pattern(nr, pointer) => isJsonPointer(pointer)
            case _                    => false
          }
        )
      // Some(v => !v.isBlank && !v.startsWith("/") && Pointer.parse("/" + v).isDefined)
      case "date-time" =>
        // https://datatracker.ietf.org/doc/html/rfc3339
        Some(v => Try(DateTimeFormatter.ISO_DATE_TIME.parse(v)).isSuccess)
      case "date" =>
        // https://datatracker.ietf.org/doc/html/rfc3339
        Some(v => Try(DateTimeFormatter.ISO_DATE.parse(v)).isSuccess)
      case "time" =>
        // https://datatracker.ietf.org/doc/html/rfc3339
        Some(v =>
          Try(DateTimeFormatter.ISO_OFFSET_TIME.parse(v))
            // .orElse(Try(DateTimeFormatter.ISO_LOCAL_TIME.parse(v)))
            .isSuccess
        )
      case "duration" =>
        // https://datatracker.ietf.org/doc/html/rfc3339
        Some(v => Try(Duration.parse(v)).isSuccess)
      case _ =>
        // TODO
//        value => combiner.valid(UnknownFormat(format), value.pointer)
        None

  private def isJsonPointer(v: String): Boolean =
    v.isBlank || (v.startsWith("/") && Pointer.parse(v).isDefined)

end Formats
