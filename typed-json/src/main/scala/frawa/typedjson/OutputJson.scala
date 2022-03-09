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

package frawa.typedjson

import frawa.typedjson.parser.Value
import frawa.typedjson.parser.Value._
import frawa.typedjson.validation.ValidationError

object OutputJson {

  def flag(validation: TypedJson.Validation): Value = {
    ObjectValue(Map("valid" -> BoolValue(validation.valid)))
  }

  def basic(validation: TypedJson.Validation): Value = {
    val errors = if (validation.output.errors.isEmpty) {
      Map()
    } else {
      Map("errors" -> ArrayValue(validation.output.errors.map(toJson)))
    }
    ObjectValue(Map("valid" -> BoolValue(validation.valid)) ++ errors)
  }

  private def toJson(error: TypedJson.Error): Value = {
    ObjectValue(
      Map(
        "error" -> StringValue(toMessage(error.error)),
        // "keywordLocation"         -> StringValue(""),
        // "absoluteKeywordLocation" -> StringValue(""),
        "instanceLocation" -> StringValue(error.pointer.toString)
      )
    )
  }

  private def toMessage(error: ValidationError): String = {
    error.toString
  }
}
