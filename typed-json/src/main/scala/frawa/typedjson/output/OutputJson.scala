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

package frawa.typedjson.output

import frawa.typedjson.parser.Value
import frawa.typedjson.parser.Value.*
import frawa.typedjson.validation.ValidationError
import frawa.typedjson.output.BasicOutput
import frawa.typedjson.TypedJson
import frawa.typedjson.keywords.KeywordLocation.{Local, Dereferenced}
import frawa.typedjson.pointer.Pointer
import java.net.URI
import frawa.typedjson.keywords.KeywordLocation
import frawa.typedjson.validation.MissingRequiredProperties
import frawa.typedjson.validation.MinItemsMismatch
import frawa.typedjson.validation.AdditionalPropertyInvalid
import frawa.typedjson.validation.FalseSchemaReason
import frawa.typedjson.validation.SubSchemaFailed
import frawa.typedjson.validation.CannotResolve
import frawa.typedjson.validation.TypeMismatch
import frawa.typedjson.validation.NotOneOf
import frawa.typedjson.validation.NotInvalid
import frawa.typedjson.validation.NotInEnum
import frawa.typedjson.validation.PatternMismatch
import frawa.typedjson.validation.FormatMismatch
import frawa.typedjson.validation.MinimumMismatch
import frawa.typedjson.validation.ItemsNotUnique
import frawa.typedjson.validation.NotMultipleOf
import frawa.typedjson.validation.MaximumMismatch
import frawa.typedjson.validation.MaxLengthMismatch
import frawa.typedjson.validation.MinLengthMismatch
import frawa.typedjson.validation.MaxItemsMismatch
import frawa.typedjson.validation.MaxPropertiesMismatch
import frawa.typedjson.validation.MinPropertiesMismatch
import frawa.typedjson.validation.NotContains
import frawa.typedjson.validation.DependentRequiredMissing
import frawa.typedjson.validation.CannotResolveDynamic
import frawa.typedjson.util.ShowValue.prettyPrint

object OutputJson:

  def flag(validation: TypedJson.Validation): Value =
    ObjectValue(Map("valid" -> BoolValue(validation.valid)))

  def basic(o: BasicOutput): Value =
    if o.isValid then
      val props = Seq("valid" -> BoolValue(o.isValid))
      ObjectValue(Map.from(props))
    else
      val error = ObjectValue(
        Map.from(
          Seq(
            toJson(o.error.getOrElse(SubSchemaFailed())),
            toJson(o.instanceLocation)
          ) ++
            toJson(o.keywordLocation.getOrElse(KeywordLocation.empty))
        )
      )
      val errors = o.errors.map { error =>
        val props = Seq(toJson(error.error), toJson(error.instanceLocation)) ++ toJson(error.keywordLocation)
        ObjectValue(Map.from(props))
      }
      val props = Seq("valid" -> BoolValue(o.isValid), "errors" -> ArrayValue(error +: errors))
      ObjectValue(Map.from(props))

  def detailed(o: DetailedOutput): Value =
    def toErrorAndErrors(o: DetailedOutput): Seq[(String, Value)] =
      val error =
        o.error.map(toJson(_)).toSeq ++
          Seq(toJson(o.instanceLocation)) ++
          toJson(o.keywordLocation.getOrElse(KeywordLocation.empty))
      val errors = o.errors
        .filterNot(_.isValid)
        .map { error =>
          ("valid" -> BoolValue(false)) +:
            toErrorAndErrors(error)
        }
        .map(Map.from(_))
        .map(ObjectValue(_))
      error ++ (if errors.nonEmpty
                then Seq(("errors" -> ArrayValue(errors)))
                else Seq())

    val errorOrErrors = toErrorAndErrors(o)
    ObjectValue(Map.from(("valid" -> BoolValue(o.isValid)) +: errorOrErrors))

  private def toJson(error: ValidationError): (String, Value) =
    "error" -> StringValue(toMessage(error))

  private def toJson(instanceLocation: Pointer): (String, Value) =
    "instanceLocation" -> StringValue(instanceLocation.toString)

  private def toJson(keywordLocation: KeywordLocation): Seq[(String, Value)] =
    keywordLocation match {
      case Dereferenced(relative, absolute) =>
        Seq(
          "keywordLocation"         -> StringValue(relative.toString),
          "absoluteKeywordLocation" -> StringValue(absolute.toString)
        )
      case Local(relative) =>
        Seq(
          "keywordLocation" -> StringValue(relative.toString)
        )
    }

private def toMessage(error: ValidationError): String =
  def quotedItems(is: Seq[String]): String =
    is.map(i => s"'${i}'").mkString(", ")

  error match {
    case SubSchemaFailed()                 => "A subschema had errors."
    case FalseSchemaReason()               => "Always invalid."
    case TypeMismatch(expected)            => s"Wrong type, expecting '${expected}."
    case NotOneOf(valid)                   => s"Expected one, but found '${valid} valid."
    case NotInvalid()                      => "Expected invalid, but found valid."
    case NotInEnum(values)                 => s"Not in enum values: ${quotedItems(values.map(prettyPrint(2)))}."
    case MissingRequiredProperties(Seq(p)) => s"Required property '${p}' not found."
    case MissingRequiredProperties(ps)     => s"Required properties ${quotedItems(ps)} not found."
    case AdditionalPropertyInvalid(p)      => s"Additional property '${p}' found but was invalid."
    case PatternMismatch(pattern)          => s"Does not match pattern '${pattern}'."
    case FormatMismatch(format)            => s"Not of format '${format}'."
    case MinimumMismatch(min, exclude) =>
      if exclude then s"Expected greater than ${min}."
      else s"Expected greater than or equal to ${min}."
    case MaximumMismatch(max, exclude) =>
      if exclude then s"Expected less than ${max}."
      else s"Expected less than or equal to ${max}."
    case ItemsNotUnique()                  => "Items expected to be unique."
    case NotMultipleOf(n)                  => s"Expected to be multiple of ${n}."
    case MaxLengthMismatch(max)            => s"Expected maximal length of ${max}."
    case MinLengthMismatch(min)            => s"Expected minimal length of ${min}."
    case MaxItemsMismatch(max, found)      => s"Expected at most ${max} items but found ${found}."
    case MinItemsMismatch(min, found)      => s"Expected at least ${min} items but found ${found}."
    case MaxPropertiesMismatch(max, found) => s"Expected at most ${max} properties but found ${found}."
    case MinPropertiesMismatch(min, found) => s"Expected at least ${min} properties but found ${found}."
    case DependentRequiredMissing(missing) =>
      s"Missing dependend required properties: ${missing
          .map { (p, vs) =>
            s"${p} requires ${quotedItems(vs)}"
          }
          .mkString(", ")}"
    case NotContains(valid)                         => s"Expected to contain ${valid} items."
    case CannotResolve(ref, problems)               => s"Cannot resolve ${ref}: ${problems.mkString(", ")}"
    case CannotResolveDynamic(ref, scope, problems) => s"Cannot resolve dynamically ${ref}: ${problems.mkString(", ")}"
  }
