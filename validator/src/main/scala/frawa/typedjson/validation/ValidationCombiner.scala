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

import frawa.typedjson.keywords.{Result, Pointer}

class ValidationCombiner extends Combiner[ValidationResult] {
  override def allOf(results: Seq[Result[ValidationResult]], pointer: Pointer): Result[ValidationResult] = {
    if (results.isEmpty || results.forall(_.valid)) {
      Result.valid
    } else {
      invalid(results)
    }
  }

  private def invalid(results: Seq[Result[ValidationResult]]): Result[ValidationResult] = {
    val errors = results.flatMap(_.results.flatMap(_.errors))
    if (errors.isEmpty) {
      Result.invalid
    } else {
      Result.invalid(ValidationResult.invalid(errors))
    }
  }

  override def invalid(observation: Observation, pointer: Pointer): Result[ValidationResult] = Result.invalid(
    ValidationResult.invalid(observation, pointer)
  )

  override def anyOf(results: Seq[Result[ValidationResult]], pointer: Pointer): Result[ValidationResult] = {
    if (results.isEmpty || results.exists(_.valid)) {
      Result.valid
    } else {
      invalid(results)
    }
  }

  override def oneOf(results: Seq[Result[ValidationResult]], pointer: Pointer): Result[ValidationResult] = {
    val valids = results.filter(_.valid)
    if (valids.size == 1) {
      Result.valid
    } else if (valids.isEmpty) {
      invalid(results)
    } else {
      invalid(NotOneOf(valids.size), pointer)
    }
  }

  override def contains(
      results: Seq[Result[ValidationResult]],
      pointer: Pointer,
      min: Option[Int],
      max: Option[Int]
  ): Result[ValidationResult] = {
    val count = results.count(_.valid)
    if (min.getOrElse(1) <= count && !max.exists(count > _)) {
      Result.valid
    } else {
      invalid(NotContains(count), pointer)
    }
  }

  override def not(results: Seq[Result[ValidationResult]], pointer: Pointer): Result[ValidationResult] = {
    if (results.length == 1 && !results.head.valid) {
      Result.valid
    } else {
      invalid(NotInvalid(), pointer)
    }
  }

  override def ifThenElse(
      results: Seq[Result[ValidationResult]],
      pointer: Pointer
  ): Result[ValidationResult] = {
    allOf(results, pointer)
  }

}
