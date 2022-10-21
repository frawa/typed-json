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

import frawa.typedjson.keywords.Result
import frawa.typedjson.pointer.Pointer

class ValidationCombiner extends Combiner[ValidationOutput] {
  private implicit val f: Result.OutputCombiner[ValidationOutput] = ValidationOutput.add

  override def allOf(results: Seq[Result[ValidationOutput]], pointer: Pointer): Result[ValidationOutput] = {
    if results.isEmpty || results.forall(_.valid) then {
      Result.valid
    } else {
      invalid(results)
    }
  }

  private def invalid(results: Seq[Result[ValidationOutput]]): Result[ValidationOutput] = {
    val errors = results.flatMap(_.output).flatMap(_.errors)
    if errors.isEmpty then {
      Result.invalid
    } else {
      Result.invalid(ValidationOutput.invalid(errors))
    }
  }
  override def valid(annotation: ValidationAnnotation, pointer: Pointer): Result[ValidationOutput] =
    Result.valid(ValidationOutput.valid(annotation, pointer))

  override def invalid(error: ValidationError, pointer: Pointer): Result[ValidationOutput] = Result.invalid(
    ValidationOutput.invalid(error, pointer)
  )

  override def anyOf(results: Seq[Result[ValidationOutput]], pointer: Pointer): Result[ValidationOutput] = {
    if results.isEmpty || results.exists(_.valid) then {
      Result.valid
    } else {
      invalid(results)
    }
  }

  override def oneOf(results: Seq[Result[ValidationOutput]], pointer: Pointer): Result[ValidationOutput] = {
    val valids = results.filter(_.valid)
    if valids.size == 1 then {
      Result.valid
    } else if valids.isEmpty then {
      invalid(results)
    } else {
      invalid(NotOneOf(valids.size), pointer)
    }
  }

  override def contains(
      results: Seq[Result[ValidationOutput]],
      pointer: Pointer,
      min: Option[Int],
      max: Option[Int]
  ): Result[ValidationOutput] = {
    val count = results.count(_.valid)
    if min.getOrElse(1) <= count && !max.exists(count > _) then {
      Result.valid
    } else {
      invalid(NotContains(count), pointer)
    }
  }

  override def not(results: Seq[Result[ValidationOutput]], pointer: Pointer): Result[ValidationOutput] = {
    if results.length == 1 && !results.head.valid then {
      Result.valid
    } else {
      invalid(NotInvalid(), pointer)
    }
  }

  override def ifThenElse(
      results: Seq[Result[ValidationOutput]],
      pointer: Pointer
  ): Result[ValidationOutput] = {
    allOf(results, pointer)
  }

}
