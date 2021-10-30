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

package frawa.typedjson.schema

import frawa.typedjson.parser.Value
import frawa.typedjson.parser.ZioParser
import frawa.typedjson.parser.Parser
import frawa.typedjson.parser.NumberValue
import frawa.typedjson.parser.BoolValue
import frawa.typedjson.parser.NullValue
import frawa.typedjson.parser.StringValue
import frawa.typedjson.parser.ArrayValue
import frawa.typedjson.parser.ObjectValue

import java.net.URI
import scala.reflect.ClassTag

class ValidationCalculator extends Calculator[ValidationResult] {
  override def allOf(checked: Seq[Checked[ValidationResult]], pointer: Pointer): Checked[ValidationResult] = {
    if (checked.isEmpty || checked.forall(_.valid)) {
      Checked.valid.count(checked)
    } else {
      invalid(checked)
    }
  }

  private def invalid(checked: Seq[Checked[ValidationResult]]): Checked[ValidationResult] =
    Checked.invalid(ValidationResult.invalid(checked.flatMap(_.results.flatMap(_.errors)))).count(checked)

  override def invalid(observation: Observation, pointer: Pointer): Checked[ValidationResult] = Checked.invalid(
    ValidationResult.invalid(observation, pointer)
  )

  override def anyOf(checked: Seq[Checked[ValidationResult]], pointer: Pointer): Checked[ValidationResult] = {
    if (checked.isEmpty || checked.exists(_.valid)) {
      Checked.valid.count(checked)
    } else {
      invalid(checked)
    }
  }

  override def oneOf(checked: Seq[Checked[ValidationResult]], pointer: Pointer): Checked[ValidationResult] = {
    val count = checked.count(_.valid)
    if (count == 1) {
      Checked.valid.count(checked)
    } else if (count == 0) {
      invalid(checked)
    } else {
      invalid(NotOneOf(count), pointer)
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
      Checked.valid.count(checked)
    } else {
      invalid(NotContains(count), pointer)
    }
  }

  override def not(checked: Seq[Checked[ValidationResult]], pointer: Pointer): Checked[ValidationResult] = {
    if (checked.length == 1 && !checked(0).valid) {
      Checked.valid.count(checked)
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
