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

object OutputJson:

  def flag(validation: TypedJson.Validation): Value =
    ObjectValue(Map("valid" -> BoolValue(validation.valid)))

  def basic(o: BasicOutput): Value =
    val error =
      o.error.map { error =>
        BasicOutput.Error(error, o.instanceLocation, o.keywordLocation.getOrElse(KeywordLocation.empty))
      }
    val errors = (error.toSeq ++ o.errors)
    val es =
      if errors.isEmpty then Map.empty
      else if errors.size == 1 then Map("error" -> error.map(toJson).get)
      else Map("errors"                         -> ArrayValue(errors.map(toJson)))
    ObjectValue(Map("valid" -> BoolValue(o.isValid)) ++ es)

  // private case class BasicError(
  //     error: ValidationError,
  //     instanceLocation: Pointer,
  //     keywordLocation: KeywordLocation
  // )

  // private def toBasicError(o: BasicOutput): Seq[BasicError] =
  //   // given Ordering[KeywordLocation] with
  //   //   def compare(x: KeywordLocation, y: KeywordLocation): Int =
  //   //     toString(x).compare(toString(y))

  //   //   private def toString(kl: KeywordLocation): String = kl match {
  //   //     case Local(relative)           => relative.toString
  //   //     case Dereferenced(relative, _) => relative.toString
  //   //   }

  //   val error = o.error.map { error =>
  //     BasicOutput.Error(error, o.instanceLocation, o.keywordLocation)
  //   }.toSeq
  //   (error ++ o.errors).sortBy(_.instanceLocation.toString)

  private def toJson(error: BasicOutput.Error): Value =
    error.keywordLocation match {
      case Dereferenced(relative, absolute) =>
        ObjectValue(
          Map(
            "keywordLocation"         -> StringValue(relative.toString),
            "absoluteKeywordLocation" -> StringValue(absolute.toString),
            "instanceLocation"        -> StringValue(error.instanceLocation.toString),
            "error"                   -> StringValue(toMessage(error.error))
          )
        )
      case Local(relative) =>
        ObjectValue(
          Map(
            "keywordLocation"  -> StringValue(relative.toString),
            "instanceLocation" -> StringValue(error.instanceLocation.toString),
            "error"            -> StringValue(toMessage(error.error))
          )
        )
    }

  private def toMessage(error: ValidationError): String =
    // TODO human messages
    error.toString
