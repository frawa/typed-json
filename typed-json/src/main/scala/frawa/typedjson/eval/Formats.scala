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

package frawa.typedjson.eval

import frawa.typedjson.parser.Value
import frawa.typedjson.pointer.Pointer
import frawa.typedjson.validation.{FormatMismatch, UnknownFormat}

import java.net.URI
import java.util.UUID
import java.util.regex.Pattern
import scala.util.Try
import java.net.IDN

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
        // val regex =
        //   """^(([a-zA-Z\\d]|[a-zA-Z\\d][a-zA-Z\\d\-]*[a-zA-Z\\d])\.)*([A-Za-z\\d]|[A-Za-z\\d][A-Za-z\\d\-]*[A-Za-z\\d])$""".r
        Some(v => Try(IDN.toASCII(v, IDN.USE_STD3_ASCII_RULES)).toOption.isDefined)
      case "idn-hostname" =>
        // see https://stackoverflow.com/questions/11809631/fully-qualified-domain-name-validation/26618995
        val regex =
          """(?=^.{4,253}$)(^((?!-)[a-zA-Z\\d-]{1,63}(?<!-)\.)+[a-zA-Z]{2,63}$)""".r
        Some(v => regex.matches(v))
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
        val regex_date = "\\d{4}-\\d{2}-\\d{2}"
        val regex_time = "\\d{2}:\\d{2}:\\d{2}(\\.\\d+)?(Z|([+\\-])\\d{2}:\\d{2})"
        val date_time  = s"${regex_date}T$regex_time"
        Some(v => date_time.r.matches(v))
      case "date" =>
        // https://datatracker.ietf.org/doc/html/rfc3339
        val `regex-date` =
          "\\d{4}-\\d{2}-\\d{2}".r
        Some(v => `regex-date`.matches(v))
      case "time" =>
        // https://datatracker.ietf.org/doc/html/rfc3339
        val `regex-time` =
          "(\\d{2}):(\\d{2}):(\\d{2})(\\.\\d+)?(Z|([+\\-]\\d{2}:\\d{2}))".r
        Some(v =>
          `regex-time`
            .findFirstMatchIn(v)
            .exists { m =>
              val hour   = m.group(1).toInt
              val minute = m.group(2).toInt
              val second = m.group(3).toInt
              Range.inclusive(0, 23).contains(hour) &&
              Range.inclusive(0, 59).contains(minute) &&
              Range.inclusive(0, 59).contains(second)
            }
        )
      //          Try {
      ////            val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss[.SSS]XXX")
      //            val formatter = DateTimeFormatter
      //              .ofPattern("HH:mm:ss[.SSS]XXX")
      //              .withResolverStyle(ResolverStyle.STRICT);
      //            OffsetTime.parse(v, formatter)
      //          }.recover { ex =>
      //            println("FW ?", ex)
      //            throw ex
      //          }.isSuccess
      case "duration" =>
        // https://datatracker.ietf.org/doc/html/rfc3339
        // TODO test me
        val `dur-second` = "\\d+S"
        val `dur-minute` = s"\\d+M(${`dur-second`})?"
        val `dur-hour`   = s"\\d+H(${`dur-minute`})?"
        val `dur-day`    = "\\d+D"
        val `dur-time`   = s"T(${`dur-hour`}|${`dur-minute`}|${`dur-second`})"
        val `dur-week`   = "\\d+W"
        val `dur-month`  = s"\\d+M(${`dur-day`})?"
        val `dur-year`   = s"\\d+Y(${`dur-month`})?"
        val `dur-date`   = s"(${`dur-day`}|${`dur-month`}|${`dur-year`})(${`dur-time`})?"
        val duration     = s"P(${`dur-date`}|${`dur-time`}|${`dur-week`})"
        Some(v => duration.r.matches(v))
      case _ =>
        // TODO
//        value => combiner.valid(UnknownFormat(format), value.pointer)
        None

  private def isJsonPointer(v: String): Boolean =
    v.isBlank || (v.startsWith("/") && Pointer.parse(v).isDefined)

end Formats

// {`dur-minute`})?"
//         val `dur-day`    = "\\d+D"
//         val `dur-time`   = s"T(${`dur-hour`}|${`dur-minute`}|${`dur-second`})"
//         val `dur-week`   = "\\d+W"
//         val `dur-month`  = s"\\d+M(${`dur-day`})?"
//         val `dur-year`   = s"\\d+Y(${`dur-month`})?"
//         val `dur-date`   = s"(${`dur-day`}|${`dur-month`}|${`dur-year`})(${`dur-time`})?"
//         val duration     = s"P(${`dur-date`}|${`dur-time`}|${`dur-week`})"
//         Some(v => duration.r.matches(v))
//       case _ =>
//         // TODO
// //        value => combiner.valid(UnknownFormat(format), value.pointer)
//         None

// }
