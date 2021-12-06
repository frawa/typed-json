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

package frawa.typedjson.suggestion

import frawa.typedjson.parser._
import frawa.typedjson.keywords._
import frawa.typedjson.validation.{ValidationProcessing, ValidationResult}

case class SuggestionResult(suggestions: Seq[Value], validated: Result[ValidationResult])

object SuggestionProcessing {

  def apply(at: Pointer): Processing[SuggestionResult] = Processing(simple(at), nested(at))

  private def simple(at: Pointer)(check: AssertionKeyword)(value: InnerValue): Result[SuggestionResult] = {
    if (at == value.pointer) {
      val suggestions = suggestFor(check)(Seq(Result.valid))
      Result.valid(SuggestionResult(suggestions, Result.valid))
    } else {
      val result = ValidationProcessing().simple(check)(value)
      Result(result.valid, SuggestionResult(Seq(), result))
    }
  }

  private def nested(
      at: Pointer
  )(check: ApplicatorKeyword)(result: Seq[Result[SuggestionResult]])(value: InnerValue): Result[SuggestionResult] = {
    if (at == value.pointer) {
      val suggestions = suggestFor(check)(result)
      Result.valid(SuggestionResult(suggestions, Result.valid))
    } else {
      val suggestions = result.flatMap(_.results).flatMap(_.suggestions)
      val validated   = result.flatMap(_.results).map(_.validated)
      val nested      = ValidationProcessing().nested(check)(validated)(value)
      Result.valid(SuggestionResult(suggestions, nested))
    }
  }

  private def suggestFor(keyword: Keyword)(results: Seq[Result[SuggestionResult]]): Seq[Value] = {
    keyword match {
      case NullTypeKeyword                           => Seq(NullValue)
      case BooleanTypeKeyword                        => Seq(BoolValue(true))
      case StringTypeKeyword                         => Seq(StringValue(""))
      case NumberTypeKeyword                         => Seq(NumberValue(0))
      case ArrayTypeKeyword                          => Seq(ArrayValue(Seq()))
      case ObjectTypeKeyword                         => Seq(ObjectValue(Map()))
      case ObjectPropertiesKeyword(properties, _, _) =>
        // TODO
        properties.flatMap { case (prop, keywords) =>
          keywords.keywords
            .flatMap(keyword => suggestFor(keyword.value)(results))
            .map(v => ObjectValue(Map(prop -> v)))
        }.toSeq
      case ObjectRequiredKeyword(required) => Seq(ObjectValue(Map.from(required.map((_, NullValue)))))
      case TrivialKeyword(_)               => Seq()
      case IfThenElseKeyword(ifChecks, thenChecks, elseChecks) =>
        Seq(ifChecks, thenChecks, elseChecks).flatten
          .flatMap(_.keywords)
          .flatMap(check => suggestFor(check.value)(results))
      case EnumKeyword(values) => values
      case _                   => results.flatMap(_.results).flatMap(_.suggestions)
    }
  }
}
