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
import frawa.typedjson.processor._
import frawa.typedjson.validation.{ValidationChecker, ValidationResult}

case class SuggestionResult(suggestions: Seq[Value], validated: Result[ValidationResult])

object SuggestionChecker {

  def apply(at: Pointer): Checker[SuggestionResult] = Checker(check(at), nested(at))

  private def check(at: Pointer)(check: SimpleKeyword)(value: InnerValue): Result[SuggestionResult] = {
    if (at == value.pointer) {
      val suggestions = suggestFor(check)(Seq(Result.valid))
      Result.valid(SuggestionResult(suggestions, Result.valid))
    } else {
      val checked = ValidationChecker().simple(check)(value)
      Result(checked.valid, SuggestionResult(Seq(), checked))
    }
  }

  private def nested(
      at: Pointer
  )(check: NestingKeyword)(checked: Seq[Result[SuggestionResult]])(value: InnerValue): Result[SuggestionResult] = {
    if (at == value.pointer) {
      val suggestions = suggestFor(check)(checked)
      Result.valid(SuggestionResult(suggestions, Result.valid))
    } else {
      val suggestions = checked.flatMap(_.results).flatMap(_.suggestions)
      val validated   = checked.flatMap(_.results).map(_.validated)
      val nested      = ValidationChecker().nested(check)(validated)(value)
      Result.valid(SuggestionResult(suggestions, nested))
    }
  }

  private def suggestFor(check: Keyword)(checked: Seq[Result[SuggestionResult]]): Seq[Value] = {
    check match {
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
            .flatMap(keyword => suggestFor(keyword.value)(checked))
            .map(v => ObjectValue(Map(prop -> v)))
        }.toSeq
      case ObjectRequiredKeyword(required) => Seq(ObjectValue(Map.from(required.map((_, NullValue)))))
      case TrivialKeyword(valid)           => Seq()
      // case AllOfCheck(checks)            => checks.flatMap(_.checks).flatMap(suggestFor(_))
      // case AnyOfCheck(checks)            => checks.flatMap(_.checks).flatMap(suggestFor(_))
      // case OneOfCheck(checks)            => checks.flatMap(_.checks).flatMap(suggestFor(_))
      case IfThenElseKeyword(ifChecks, thenChecks, elseChecks) =>
        Seq(ifChecks, thenChecks, elseChecks)
          .flatMap(identity)
          .flatMap(_.keywords)
          .flatMap(check => suggestFor(check.value)(checked))
      // case UnionTypeCheck(checks) => checks.flatMap(suggestFor(_)(checked))
      case EnumKeyword(values) => values
      case _                   => checked.flatMap(_.results).flatMap(_.suggestions)
    }
  }
}
