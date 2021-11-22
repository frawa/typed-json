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

package frawa.typedjson.validation

import frawa.typedjson.parser._
import frawa.typedjson.processor._

import java.net.URI
//import java.time.OffsetTime
//import java.time.format.{DateTimeFormatter, ResolverStyle}
import java.util.UUID
import java.util.regex.Pattern
import scala.reflect.ClassTag
import scala.util.Try

// TODO rename to something like "Error"
sealed trait Observation
case class FalseSchemaReason()                                         extends Observation
case class TypeMismatch[T <: Value](expected: String)                  extends Observation
case class NotOneOf(valid: Int)                                        extends Observation
case class NotInvalid()                                                extends Observation
case class NotInEnum(values: Seq[Value])                               extends Observation
case class MissingRequiredProperties(properties: Seq[String])          extends Observation
case class PatternMismatch(pattern: String)                            extends Observation
case class FormatMismatch(format: String)                              extends Observation
case class MinimumMismatch(min: BigDecimal, exclude: Boolean)          extends Observation
case class ItemsNotUnique()                                            extends Observation
case class UnsupportedFormat(format: String)                           extends Observation
case class UnsupportedCheck(check: Check)                              extends Observation
case class NotMultipleOf(n: BigDecimal)                                extends Observation
case class MaximumMismatch(max: BigDecimal, exclude: Boolean)          extends Observation
case class MaxLengthMismatch(max: BigDecimal)                          extends Observation
case class MinLengthMismatch(min: BigDecimal)                          extends Observation
case class MaxItemsMismatch(max: BigDecimal)                           extends Observation
case class MinItemsMismatch(min: BigDecimal)                           extends Observation
case class MaxPropertiesMismatch(max: BigDecimal)                      extends Observation
case class MinPropertiesMismatch(min: BigDecimal)                      extends Observation
case class DependentRequiredMissing(missing: Map[String, Seq[String]]) extends Observation
case class NotContains(valid: Int)                                     extends Observation

trait Calculator[R] {
  def invalid(observation: Observation, pointer: Pointer): Checked[R]
  def allOf(checked: Seq[Checked[R]], pointer: Pointer): Checked[R]
  def anyOf(checked: Seq[Checked[R]], pointer: Pointer): Checked[R]
  def oneOf(checked: Seq[Checked[R]], pointer: Pointer): Checked[R]
  def contains(checked: Seq[Checked[R]], pointer: Pointer, min: Option[Int], max: Option[Int]): Checked[R]
  def not(checked: Seq[Checked[R]], pointer: Pointer): Checked[R]
  def ifThenElse(checked: Seq[Checked[R]], pointer: Pointer): Checked[R]
}
object ValidationChecker {

  def apply(): Checker[ValidationResult] = Checker(check, nested)

  private val calc: Calculator[ValidationResult] = new ValidationCalculator()

  private val nullTypeMismatch    = TypeMismatch[NullValue.type]("null")
  private val booleanTypeMismatch = TypeMismatch[BoolValue]("boolean")
  private val stringTypeMismatch  = TypeMismatch[StringValue]("string")
  private val numberTypeMismatch  = TypeMismatch[NumberValue]("number")
  private val arrayTypeMismatch   = TypeMismatch[ArrayValue]("array")
  private val objectTypeMismatch  = TypeMismatch[ObjectValue]("object")

  private type ProcessFun = Processor.ProcessFun[ValidationResult]

  private def check(check: SimpleCheck): ProcessFun = {
    check match {
      case NullTypeCheck                 => checkType(nullTypeMismatch)
      case BooleanTypeCheck              => checkType(booleanTypeMismatch)
      case StringTypeCheck               => checkType(stringTypeMismatch)
      case NumberTypeCheck               => checkType(numberTypeMismatch)
      case IntegerTypeCheck              => checkInteger()
      case ArrayTypeCheck                => checkType(arrayTypeMismatch)
      case ObjectTypeCheck               => checkType(objectTypeMismatch)
      case ObjectRequiredCheck(required) => checkObjectRequired(required)
      case TrivialCheck(valid)           => checkTrivial(valid)
      case EnumCheck(values)             => checkEnum(values)
      case PatternCheck(pattern)         => checkPattern(pattern)
      case FormatCheck(format)           => checkFormat(format)
      case MinimumCheck(v, exclude)      => checkMinimum(v, exclude)
      case UniqueItemsCheck(v)           => checkUniqueItems(v)
      case MultipleOfCheck(n)            => checkMultipleOf(n)
      case MaximumCheck(v, exclude)      => checkMaximum(v, exclude)
      case MaxLengthCheck(v)             => checkMaxLength(v)
      case MinLengthCheck(v)             => checkMinLength(v)
      case MaxItemsCheck(v)              => checkMaxItems(v)
      case MinItemsCheck(v)              => checkMinItems(v)
      case MaxPropertiesCheck(v)         => checkMaxProperties(v)
      case MinPropertiesCheck(v)         => checkMinProperties(v)
      case DependentRequiredCheck(v)     => checkDependentRequired(v)
      case _                             => _ => Checked.invalid(ValidationResult.invalid(UnsupportedCheck(check)))
    }
  }

  private def nested(check: NestingCheck)(checked: Seq[Checked[ValidationResult]]): ProcessFun = { value =>
    check match {
      case AllOfCheck(_)                  => calc.allOf(checked, value.pointer)
      case AnyOfCheck(_)                  => calc.anyOf(checked, value.pointer)
      case OneOfCheck(_)                  => calc.oneOf(checked, value.pointer)
      case NotCheck(_)                    => calc.not(checked, value.pointer)
      case UnionTypeCheck(_)              => calc.oneOf(checked, value.pointer)
      case ObjectPropertiesCheck(_, _, _) => calc.allOf(checked, value.pointer)
      case ArrayItemsCheck(_, _)          => calc.allOf(checked, value.pointer)
      case IfThenElseCheck(_, _, _)       => calc.ifThenElse(checked, value.pointer)
      case PropertyNamesCheck(_)          => calc.allOf(checked, value.pointer)
      case c: LazyResolveCheck            => calc.allOf(checked, value.pointer)
      case DependentSchemasCheck(_)       => calc.allOf(checked, value.pointer)
      case ContainsCheck(_, min, max)     => calc.contains(checked, value.pointer, min, max)
      case c: UnevaluatedItemsCheck       => calc.allOf(checked, value.pointer)
      case c: UnevaluatedPropertiesCheck  => calc.allOf(checked, value.pointer)
    }
  }

  private def checkType[T <: Value: ClassTag](observation: TypeMismatch[T]): ProcessFun = value =>
    value.value match {
      case v: T => Checked.valid
      case _    => calc.invalid(observation, value.pointer)
    }

  private def checkInteger(): ProcessFun = value =>
    value.value match {
      case NumberValue(v) =>
        if (v.isValidLong)
          Checked.valid
        else
          calc.invalid(TypeMismatch[NumberValue]("integer"), value.pointer)
      case _ => calc.invalid(TypeMismatch[NumberValue]("integer"), value.pointer)
    }

  private def checkTrivial(valid: Boolean): ProcessFun = { value =>
    if (valid)
      Checked.valid
    else
      calc.invalid(FalseSchemaReason(), value.pointer)
  }

  private def checkObjectRequired(required: Seq[String]): ProcessFun = { value =>
    value.value match {
      case ObjectValue(propertiesValues) => {
        val missingNames = required.filter(!propertiesValues.contains(_))
        if (missingNames.isEmpty) {
          Checked.valid
        } else {
          calc.invalid(MissingRequiredProperties(missingNames), value.pointer)
        }
      }
      case _ => Checked.valid
    }
  }

  private def checkEnum(values: Seq[Value]): ProcessFun = { value =>
    if (values.contains(value.value)) {
      Checked.valid
    } else {
      calc.invalid(NotInEnum(values), value.pointer)
    }
  }

  private def checkPattern(pattern: String): ProcessFun = {
    val r = pattern.r
    return { value =>
      value.value match {
        case StringValue(v) =>
          if (r.findFirstIn(v).isDefined)
            Checked.valid
          else
            calc.invalid(PatternMismatch(pattern), value.pointer)
        case _ => Checked.valid
      }
    }
  }

  private def checkFormat(format: String): ProcessFun = {
    format match {
      case "regex" =>
        checkStringValue(FormatMismatch(format)) { v =>
          Try(Pattern.compile(v)).isSuccess
        }
      case "email" =>
        checkStringValue(FormatMismatch(format)) { v =>
          // see https://emailregex.com/
          val regex = {
            """(?:[a-z\\d!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z\\d!#$%&'*+/=?^_`{|}~-]+)*|"(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21\x23-\x5b\x5d-\x7f]|\\[\x01-\x09\x0b\x0c\x0e-\x7f])*")@(?:(?:[a-z\\d](?:[a-z\\d-]*[a-z\\d])?\.)+[a-z\\d](?:[a-z\\d-]*[a-z\\d])?|\[(?:(?:25[0-5]|2[0-4][\\d]|[01]?[\\d][\\d]?)\.){3}(?:25[0-5]|2[0-4][\\d]|[01]?[\\d][\\d]?|[a-z\\d-]*[a-z\\d]:(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21-\x5a\x53-\x7f]|\\[\x01-\x09\x0b\x0c\x0e-\x7f])+)\])""".r
          }
          regex.matches(v)
        }
      case "idn-email" =>
        checkStringValue(FormatMismatch(format)) { v =>
          // see https://stackoverflow.com/questions/13992403/regex-validation-of-email-addresses-according-to-rfc5321-rfc5322
          val regex = {
            """([!#-'*+/-9=?A-Z^-~-]+(\.[!#-'*+/-9=?A-Z^-~-]+)*|"([ ]!#-[^-~ \t]|(\\[\t -~]))+")@([!#-'*+/-9=?A-Z^-~-]+(\.[!#-'*+/-9=?A-Z^-~-]+)*|\[[\t -Z^-~]*])""".r
          }
          regex.matches(v)
        }
      case "hostname" =>
        checkStringValue(FormatMismatch(format)) { v =>
          // see https://stackoverflow.com/questions/106179/regular-expression-to-match-dns-hostname-or-ip-address
          val regex = {
            """^(([a-zA-Z\\d]|[a-zA-Z\\d][a-zA-Z\\d\-]*[a-zA-Z\\d])\.)*([A-Za-z\\d]|[A-Za-z\\d][A-Za-z\\d\-]*[A-Za-z\\d])$""".r
          }
          regex.matches(v)
        }
      case "idn-hostname" =>
        checkStringValue(FormatMismatch(format)) { v =>
          // see https://stackoverflow.com/questions/11809631/fully-qualified-domain-name-validation/26618995
          val regex = {
            """(?=^.{4,253}$)(^((?!-)[a-zA-Z\\d-]{1,63}(?<!-)\.)+[a-zA-Z]{2,63}$)""".r
          }
          regex.matches(v)
        }
      case "ipv4" =>
        // see https://stackoverflow.com/questions/106179/regular-expression-to-match-dns-hostname-or-ip-address
        checkStringValue(FormatMismatch(format)) { v =>
          val regex = {
            """^(([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])\.){3}([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])$""".r
          }
          regex.matches(v)
        }
      case "ipv6" =>
        // see https://stackoverflow.com/questions/53497/regular-expression-that-matches-valid-ipv6-addresses
        checkStringValue(FormatMismatch(format)) { v =>
          val regex = {
            """(([0-9a-fA-F]{1,4}:){7,7}[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,7}:|([0-9a-fA-F]{1,4}:){1,6}:[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,5}(:[0-9a-fA-F]{1,4}){1,2}|([0-9a-fA-F]{1,4}:){1,4}(:[0-9a-fA-F]{1,4}){1,3}|([0-9a-fA-F]{1,4}:){1,3}(:[0-9a-fA-F]{1,4}){1,4}|([0-9a-fA-F]{1,4}:){1,2}(:[0-9a-fA-F]{1,4}){1,5}|[0-9a-fA-F]{1,4}:((:[0-9a-fA-F]{1,4}){1,6})|:((:[0-9a-fA-F]{1,4}){1,7}|:)|fe80:(:[0-9a-fA-F]{0,4}){0,4}%[0-9a-zA-Z]{1,}|::(ffff(:0{1,4}){0,1}:){0,1}((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])|([0-9a-fA-F]{1,4}:){1,4}:((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]))""".r
          }
          regex.matches(v)
        }
      case "uri" =>
        checkStringValue(FormatMismatch(format)) { v =>
          Try(new URI(v)).map(_.isAbsolute).getOrElse(false)
        }
      case "uri-reference" =>
        // TODO reference?
        checkStringValue(FormatMismatch(format)) { v =>
          Try(new URI(v)).isSuccess
        }
      case "uri-template" =>
        // see https://datatracker.ietf.org/doc/html/rfc6570
        // TODO template?
        // TODO may use for iri?
        checkStringValue(FormatMismatch(format)) { v =>
          Try(new URI(v)).isSuccess
        }
      case "iri" =>
        // see https://datatracker.ietf.org/doc/html/rfc3987
        // TODO iri
        checkStringValue(FormatMismatch(format)) { v =>
          Try(new URI(v)).map(_.isAbsolute).getOrElse(false)
        }
      case "iri-reference" =>
        // see https://datatracker.ietf.org/doc/html/rfc3987
        // TODO iri
        // TODO reference?
        checkStringValue(FormatMismatch(format)) { v =>
          Try(new URI(v)).isSuccess
        }
      case "uuid" =>
        // see https://datatracker.ietf.org/doc/html/rfc4122
        checkStringValue(FormatMismatch(format)) { v =>
          Try(UUID.fromString(v)).isSuccess
        }
      case "json-pointer" =>
        checkStringValue(FormatMismatch(format)) { v =>
          Pointer.parse(v).toString().equals(v)
        }
      case "relative-json-pointer" =>
        checkStringValue(FormatMismatch(format)) { v =>
          !v.startsWith("/") && Pointer.parse("/" + v).toString().equals("/" + v)
        }
      case "date-time" =>
        // https://datatracker.ietf.org/doc/html/rfc3339
        checkStringValue(FormatMismatch(format)) { v =>
          val regex_date = "\\d{4}-\\d{2}-\\d{2}"
          val regex_time = "\\d{2}:\\d{2}:\\d{2}(\\.\\d+)?(Z|([+\\-])\\d{2}:\\d{2})"
          val date_time  = s"${regex_date}T${regex_time}"
          date_time.r.matches(v)
        }
      case "date" =>
        // https://datatracker.ietf.org/doc/html/rfc3339
        checkStringValue(FormatMismatch(format)) { v =>
          val `regex-date` = {
            "\\d{4}-\\d{2}-\\d{2}".r
          }
          `regex-date`.matches(v)
        }
      case "time" =>
        // https://datatracker.ietf.org/doc/html/rfc3339
        checkStringValue(FormatMismatch(format)) { v =>
          val `regex-time` = {
            "(\\d{2}):(\\d{2}):(\\d{2})(\\.\\d+)?(Z|([+\\-]\\d{2}:\\d{2}))".r
          }
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
        }
      case "duration" =>
        // https://datatracker.ietf.org/doc/html/rfc3339
        // TODO test me
        checkStringValue(FormatMismatch(format)) { v =>
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
          duration.r.matches(v)
        }
      case _ => { value =>
        calc.invalid(UnsupportedFormat(format), value.pointer)
      }
    }
  }

  private def checkStringValue(observation: => Observation)(check: String => Boolean): ProcessFun = { value =>
    value.value match {
      case StringValue(v) =>
        if (check(v))
          Checked.valid
        else
          calc.invalid(observation, value.pointer)
      case _ => Checked.valid
    }
  }

  private def checkMinimum(min: BigDecimal, exclude: Boolean): ProcessFun = {
    checkNumberValue(MinimumMismatch(min, exclude)) { v =>
      if (exclude) min < v else min <= v
    }
  }

  private def checkNumberValue(observation: => Observation)(check: BigDecimal => Boolean): ProcessFun = { value =>
    value.value match {
      case NumberValue(v) =>
        if (check(v))
          Checked.valid
        else
          calc.invalid(observation, value.pointer)
      case _ => Checked.valid
    }
  }

  private def checkArrayValue(observation: => Observation)(check: Seq[Value] => Boolean): ProcessFun = { value =>
    value.value match {
      case ArrayValue(v) =>
        if (check(v))
          Checked.valid
        else
          calc.invalid(observation, value.pointer)
      case _ => Checked.valid
    }
  }

  private def checkUniqueItems(unique: Boolean): ProcessFun = {
    checkArrayValue(ItemsNotUnique()) { v =>
      !unique || v.distinct.length == v.length
    }
  }

  private def checkMultipleOf(n: BigDecimal): ProcessFun = {
    checkNumberValue(NotMultipleOf(n)) { v =>
      (v / n).isValidInt
    }
  }

  private def checkMaximum(max: BigDecimal, exclude: Boolean): ProcessFun = {
    checkNumberValue(MaximumMismatch(max, exclude)) { v =>
      if (exclude) max > v else max >= v
    }
  }

  private def checkMaxLength(max: BigDecimal): ProcessFun = {
    checkStringValue(MaxLengthMismatch(max)) { v =>
      countCharPoints(v) <= max
    }
  }

  private def checkMinLength(min: BigDecimal): ProcessFun = {
    checkStringValue(MinLengthMismatch(min)) { v =>
      countCharPoints(v) >= min
    }
  }

  private def countCharPoints(text: String): Int = {
    var i     = 0
    var count = 0
    while (i < text.length()) {
      i += Character.charCount(text.codePointAt(i))
      count += 1
    }
    count
  }

  private def checkMaxItems(max: BigDecimal): ProcessFun = {
    checkArrayValue(MaxItemsMismatch(max)) { v =>
      max >= v.length
    }
  }

  private def checkMinItems(min: BigDecimal): ProcessFun = {
    checkArrayValue(MinItemsMismatch(min)) { v =>
      min <= v.length
    }
  }

  private def checkObjectValue(observation: => Observation)(check: Map[String, Value] => Boolean): ProcessFun = {
    value =>
      value.value match {
        case ObjectValue(v) =>
          if (check(v))
            Checked.valid
          else
            calc.invalid(observation, value.pointer)
        case _ => Checked.valid
      }
  }

  private def checkMaxProperties(max: BigDecimal): ProcessFun = {
    checkObjectValue(MaxPropertiesMismatch(max)) { v =>
      max >= v.keySet.size
    }
  }

  private def checkMinProperties(min: BigDecimal): ProcessFun = {
    checkObjectValue(MinPropertiesMismatch(min)) { v =>
      min <= v.keySet.size
    }
  }

  private def checkDependentRequired(required: Map[String, Seq[String]]): ProcessFun = { value =>
    value.value match {
      case ObjectValue(v) =>
        val missing = required
          .flatMap { case (property, required) =>
            v
              .get(property)
              .map { _ => required.filterNot(v.contains) }
              .filterNot(_.isEmpty)
              .map(property -> _)
          }
        if (missing.isEmpty)
          Checked.valid
        else
          calc.invalid(DependentRequiredMissing(missing), value.pointer)
      case _ => Checked.valid
    }
  }

}
