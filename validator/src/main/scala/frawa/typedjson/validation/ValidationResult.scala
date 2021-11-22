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

import frawa.typedjson.schema.{Pointer, WithPointer}

// TODO no defaults?
case class ValidationResult(
    errors: Seq[ValidationResult.Error]
)

object ValidationResult {
  type Error = WithPointer[Observation]

  def invalid(errors: Seq[Error]): ValidationResult = ValidationResult(errors)

  def invalid(observation: Observation, pointer: Pointer = Pointer.empty): ValidationResult = invalid(
    Seq(WithPointer(observation, pointer))
  )

}
