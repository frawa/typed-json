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

import frawa.typedjson.keywords._
import frawa.typedjson.parser.Value._
import frawa.typedjson.parser._
import frawa.typedjson.pointer.Pointer
import frawa.typedjson.validation.{ValidationProcessing, ValidationResult}

case class SuggestionResult(suggestions: Seq[Value], validated: Result[ValidationResult])

object SuggestionProcessing {

  def apply(at: Pointer): Processing[SuggestionResult] = Processing(simple(at), nested(at))

  private def simple(at: Pointer)(keyword: AssertionKeyword)(value: InnerValue): Result[SuggestionResult] = {
    if (at == value.pointer) {
      val suggestions = suggestFor(keyword)(Seq(Result.valid))
      Result.valid(SuggestionResult(suggestions, Result.valid))
    } else {
      val result = ValidationProcessing().simple(keyword)(value)
      Result(result.valid, SuggestionResult(Seq(), result))
    }
  }

  private def nested(
      at: Pointer
  )(keyword: ApplicatorKeyword)(results: Seq[Result[SuggestionResult]])(value: InnerValue): Result[SuggestionResult] = {
    val current = results.flatMap(_.results).flatMap(_.suggestions)
    if (at.isInsideKey && at.outer == value.pointer) {
      val suggestions = onlyKeys(suggestFor(keyword)(Seq(Result.valid)))
      Result.valid(SuggestionResult(current ++ suggestions, Result.valid))
    } else if (at == value.pointer) {
      val suggestions = suggestFor(keyword)(results)
      Result.valid(SuggestionResult(current ++ suggestions, Result.valid))
    } else {
      val validated = results.flatMap(_.results).map(_.validated)
      val nested    = ValidationProcessing().nested(keyword)(validated)(value)
      Result.valid(SuggestionResult(current, nested))
    }
  }

  private def suggestFor(keyword: Keyword)(results: Seq[Result[SuggestionResult]]): Seq[Value] = {
    keyword match {
      case NullTypeKeyword    => Seq(NullValue)
      case BooleanTypeKeyword => Seq(BoolValue(true))
      case StringTypeKeyword  => Seq(StringValue(""))
      case NumberTypeKeyword  => Seq(NumberValue(0))
      case ArrayTypeKeyword   => Seq(ArrayValue(Seq()))
      case ObjectTypeKeyword  => Seq(ObjectValue(Map()))
      case ObjectPropertiesKeyword(properties, _, _) =>
        properties.flatMap { case (prop, keywords) =>
          keywords.keywords
            .flatMap(keyword => suggestFor(keyword.value)(results))
            .map(v => ObjectValue(Map(prop -> v)))
        }.toSeq
      case ObjectRequiredKeyword(required) => Seq(ObjectValue(Map.from(required.map((_, NullValue)))))
      case TrivialKeyword(v)               => Seq(BoolValue(v))
      case IfThenElseKeyword(ifChecks, thenChecks, elseChecks) =>
        Seq(ifChecks, thenChecks, elseChecks).flatten
          .flatMap(_.keywords)
          .flatMap(keyword => suggestFor(keyword.value)(results))
      case OneOfKeyword(keywords) =>
        keywords
          .flatMap(_.keywords)
          .flatMap(keyword => suggestFor(keyword.value)(results))
      case AnyOfKeyword(keywords) =>
        keywords
          .flatMap(_.keywords)
          .flatMap(keyword => suggestFor(keyword.value)(results))
      case AllOfKeyword(keywords) =>
        // TODO intersect?
        keywords
          .flatMap(_.keywords)
          .flatMap(keyword => suggestFor(keyword.value)(results))
      case EnumKeyword(values) => values
      case ArrayItemsKeyword(items, prefixItems) =>
        val itemArrays = Seq(items).flatten
          .flatMap(_.keywords)
          .flatMap(keyword => suggestFor(keyword.value)(results))
          .map(v => ArrayValue(Seq(v)))
        // TODO combinations? might explode?
        val tuplesOfHeads = ArrayValue(
          prefixItems
            .flatMap(_.keywords)
            .map(keyword => suggestFor(keyword.value)(results))
            .map(_.headOption)
            .map(_.getOrElse(NullValue))
        )
        itemArrays :+ tuplesOfHeads
      case UnionTypeKeyword(keywords) =>
        keywords
          .flatMap(keyword => suggestFor(keyword.value)(results))
      case _ =>
        // useful for debugging:
        // Seq(StringValue(keyword.getClass.getSimpleName))
        Seq(NullValue)
    }
  }

  private def onlyKeys(suggestions: Seq[Value]): Seq[StringValue] = {
    suggestions.flatMap(Value.asObject).flatMap(_.keys).map(StringValue)
  }

}
