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

object OutputJson:

  def flag(validation: TypedJson.Validation): Value =
    ObjectValue(Map("valid" -> BoolValue(validation.valid)))

  def basic(o: BasicOutput): Value =
    if o.isValid then
      val props = Seq("valid" -> BoolValue(o.isValid))
      ObjectValue(Map.from(props))
    else
      val error = o.error
        .map { error =>
          Seq(toJson(error), toJson(o.instanceLocation)) ++
            toJson(o.keywordLocation.getOrElse(KeywordLocation.empty))
        }
        .map(props => ObjectValue(Map.from(props)))
      val errors = o.errors.map { error =>
        val props = Seq(toJson(error.error), toJson(error.instanceLocation)) ++ toJson(error.keywordLocation)
        ObjectValue(Map.from(props))
      }

      val props = Seq("valid" -> BoolValue(o.isValid), "errors" -> ArrayValue(error.toSeq ++ errors))
      ObjectValue(Map.from(props))

  def detailed(o: DetailedOutput): Value =
    def toErrorOrErrors(o: DetailedOutput): Seq[(String, Value)] =
      o.error
        .map { error =>
          Seq(toJson(error), toJson(o.instanceLocation)) ++
            toJson(o.keywordLocation.getOrElse(KeywordLocation.empty))
        }
        .getOrElse {
          val errors = o.errors
            .filterNot(_.isValid)
            .map { error =>
              ("valid" -> BoolValue(false)) +:
                toErrorOrErrors(error)
            }
            .map(Map.from(_))
            .map(ObjectValue(_))
          Seq(("errors" -> ArrayValue(errors)), toJson(o.instanceLocation)) ++
            toJson(o.keywordLocation.getOrElse(KeywordLocation.empty))
        }

    val errorOrErrors = toErrorOrErrors(o)
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
    error match {
      case MissingRequiredProperties(Seq(p)) => s"Required property '$p' not found."
      case MinItemsMismatch(min, found)      => s"Expected at least ${min} items but found ${found}."
      // TODO more!
      case _ => error.toString
    }
