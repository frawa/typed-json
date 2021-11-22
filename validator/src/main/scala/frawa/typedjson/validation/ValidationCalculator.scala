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

import frawa.typedjson.schema.{Checked, Pointer}

class ValidationCalculator extends Calculator[ValidationResult] {
  override def allOf(checked: Seq[Checked[ValidationResult]], pointer: Pointer): Checked[ValidationResult] = {
    if (checked.isEmpty || checked.forall(_.valid)) {
      Checked.valid
    } else {
      invalid(checked)
    }
  }

  private def invalid(checked: Seq[Checked[ValidationResult]]): Checked[ValidationResult] = {
    val errors = checked.flatMap(_.results.flatMap(_.errors))
    if (errors.isEmpty) {
      Checked.invalid
    } else {
      Checked.invalid(ValidationResult.invalid(errors))
    }
  }

  override def invalid(observation: Observation, pointer: Pointer): Checked[ValidationResult] = Checked.invalid(
    ValidationResult.invalid(observation, pointer)
  )

  override def anyOf(checked: Seq[Checked[ValidationResult]], pointer: Pointer): Checked[ValidationResult] = {
    if (checked.isEmpty || checked.exists(_.valid)) {
      Checked.valid
    } else {
      invalid(checked)
    }
  }

  override def oneOf(checked: Seq[Checked[ValidationResult]], pointer: Pointer): Checked[ValidationResult] = {
    val valids = checked.filter(_.valid)
    if (valids.size == 1) {
      Checked.valid
    } else if (valids.isEmpty) {
      invalid(checked)
    } else {
      invalid(NotOneOf(valids.size), pointer)
    }
  }

  override def contains(
      checked: Seq[Checked[ValidationResult]],
      pointer: Pointer,
      min: Option[Int],
      max: Option[Int]
  ): Checked[ValidationResult] = {
    val count = checked.count(_.valid)
    if (min.getOrElse(1) <= count && !max.exists(count > _)) {
      Checked.valid
    } else {
      invalid(NotContains(count), pointer)
    }
  }

  override def not(checked: Seq[Checked[ValidationResult]], pointer: Pointer): Checked[ValidationResult] = {
    if (checked.length == 1 && !checked(0).valid) {
      Checked.valid
    } else {
      invalid(NotInvalid(), pointer)
    }
  }

  override def ifThenElse(
      checked: Seq[Checked[ValidationResult]],
      pointer: Pointer
  ): Checked[ValidationResult] = {
    allOf(checked, pointer)
  }

}
