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

object OutputJson:

  def flag(validation: TypedJson.Validation): Value =
    ObjectValue(Map("valid" -> BoolValue(validation.valid)))

  def basic(o: BasicOutput): Value =
    val errors =
      if o.errors.isEmpty then Map()
      else Map("errors" -> ArrayValue(o.errors.map(toJson)))
    ObjectValue(Map("valid" -> BoolValue(o.isValid)) ++ errors)

  private def toJson(error: BasicOutput.Error): Value =
    error.keywordLocation match {
      case Local(relative) =>
        ObjectValue(
          Map(
            "error"            -> StringValue(toMessage(error.error)),
            "keywordLocation"  -> StringValue(relative.toString),
            "instanceLocation" -> StringValue(error.instanceLocation.toString)
          )
        )
      case Dereferenced(relative, absolute) =>
        ObjectValue(
          Map(
            "error"                   -> StringValue(toMessage(error.error)),
            "keywordLocation"         -> StringValue(relative.toString),
            "absoluteKeywordLocation" -> StringValue(absolute.toString),
            "instanceLocation"        -> StringValue(error.instanceLocation.toString)
          )
        )
    }

  private def toMessage(error: ValidationError): String =
    // TODO human messages
    error.toString
