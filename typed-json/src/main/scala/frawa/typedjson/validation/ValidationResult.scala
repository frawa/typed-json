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

import frawa.typedjson.keywords.WithPointer
import frawa.typedjson.pointer.Pointer

case class ValidationResult(
    errors: Seq[ValidationResult.Error],
    annotations: Seq[ValidationResult.Annotation] = Seq.empty
)

object ValidationResult {
  type Error      = WithPointer[ValidationError]
  type Annotation = WithPointer[ValidationAnnotation]

  def invalid(errors: Seq[Error]): ValidationResult = ValidationResult(errors)

  def valid(annotation: ValidationAnnotation, pointer: Pointer = Pointer.empty): ValidationResult =
    ValidationResult(Seq(), Seq(WithPointer(annotation, pointer)))

  def invalid(error: ValidationError, pointer: Pointer = Pointer.empty): ValidationResult = invalid(
    Seq(WithPointer(error, pointer))
  )
}
