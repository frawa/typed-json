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

import frawa.typedjson.keywords.*
import frawa.typedjson.parser.Value.*
import frawa.typedjson.parser.*
import frawa.typedjson.pointer.Pointer
import frawa.typedjson.validation.{ValidationProcessing, ValidationOutput}

case class SuggestionOutput(suggestions: Set[Value], validated: Result[ValidationOutput])

object SuggestionProcessing:

  def apply(at: Pointer): Processing[SuggestionOutput] = Processing(simple(at), nested(at))

  private given Result.OutputCombiner[SuggestionOutput] = add

  private def add(o1: SuggestionOutput, o2: SuggestionOutput): SuggestionOutput =
    SuggestionOutput(o1.suggestions ++ o2.suggestions, o1.validated.add(o2.validated))

  private def simple(at: Pointer)(keyword: AssertionKeyword)(value: InnerValue): Result[SuggestionOutput] =
    if at == value.pointer then
      val suggestions = suggestFor(keyword)(Seq(Result.valid))
      Result.valid(SuggestionOutput(suggestions, Result.valid[ValidationOutput]))
    else
      val result = ValidationProcessing().simple(keyword)(value)
      Result(result.valid, Some(SuggestionOutput(Set(), result)))

  private def nested(
      at: Pointer
  )(keyword: ApplicatorKeyword)(results: Seq[Result[SuggestionOutput]])(value: InnerValue): Result[SuggestionOutput] =
    val current = results.flatMap(_.output).flatMap(_.suggestions).toSet
    if at.isInsideKey && at.outer == value.pointer then
      val suggestions = onlyKeys(suggestFor(keyword)(Seq(Result.valid)))
      Result.valid(SuggestionOutput(current ++ suggestions, Result.valid))
    else if at == value.pointer then
      val suggestions = suggestFor(keyword)(results)
      Result.valid(SuggestionOutput(current ++ suggestions, Result.valid))
    else
      val validated = results.flatMap(_.output).map(_.validated)
      val nested    = ValidationProcessing().nested(keyword)(validated)(value)
      Result.valid(SuggestionOutput(current, nested))

  private def suggestFor(keyword: Keyword)(results: Seq[Result[SuggestionOutput]]): Set[Value] =
    keyword match
      case NullTypeKeyword    => Set(NullValue)
      case BooleanTypeKeyword => Set(BoolValue(true))
      case StringTypeKeyword  => Set(StringValue(""))
      case NumberTypeKeyword  => Set(NumberValue(0))
      case ArrayTypeKeyword   => Set(ArrayValue(Seq()))
      case ObjectTypeKeyword  => Set(ObjectValue(Map()))
      case ObjectPropertiesKeyword(properties, _, _) =>
        properties.flatMap { case (prop, keywords) =>
          keywords
            .flatMap(keyword => suggestFor(keyword.value)(results).toSet)
            .map(v => ObjectValue(Map(prop -> v)))
        }.toSet
      case ObjectRequiredKeyword(required) => Set(ObjectValue(Map.from(required.map((_, NullValue)))))
      case TrivialKeyword(v)               => Set(BoolValue(v))
      case IfThenElseKeyword(ifChecks, thenChecks, elseChecks) =>
        Set(ifChecks, thenChecks, elseChecks).flatten
          .flatMap(_.flatMap(keyword => suggestFor(keyword.value)(results).toSet))
      case OneOfKeyword(keywords) =>
        keywords
          .flatMap(_.flatMap(keyword => suggestFor(keyword.value)(results)))
          .toSet
      case AnyOfKeyword(keywords) =>
        keywords
          .flatMap(_.flatMap(keyword => suggestFor(keyword.value)(results)))
          .toSet
      case AllOfKeyword(keywords) =>
        // TODO intersect?
        keywords
          .flatMap(_.flatMap(keyword => suggestFor(keyword.value)(results)))
          .toSet
      case EnumKeyword(values) => values.toSet
      case ArrayItemsKeyword(items, prefixItems) =>
        val itemArrays = Seq(items).flatten
          .flatMap(_.flatMap(keyword => suggestFor(keyword.value)(results).toSet))
          .map(v => ArrayValue(Seq(v)))
        // TODO combinations? might explode?
        val tuplesOfHeads = ArrayValue(
          prefixItems
            .map(_.flatMap(keyword => suggestFor(keyword.value)(results).toSet))
            .map(_.headOption)
            .map(_.getOrElse(NullValue))
        )
        (itemArrays :+ tuplesOfHeads).toSet
      case UnionTypeKeyword(keywords) =>
        keywords
          .flatMap(keyword => suggestFor(keyword.value)(results))
          .toSet
      case _ =>
        // useful for debugging:
        // Seq(StringValue(keyword.getClass.getSimpleName))
        Set(NullValue)

  private def onlyKeys(suggestions: Set[Value]): Set[StringValue] =
    suggestions.flatMap(Value.asObject).flatMap(_.keys).map(StringValue.apply).toSet
