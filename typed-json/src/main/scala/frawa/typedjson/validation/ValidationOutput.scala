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

case class ValidationOutput(
    errors: Seq[ValidationOutput.Error],
    annotations: Seq[ValidationOutput.Annotation] = Seq.empty
)

object ValidationOutput {
  type Error      = WithPointer[ValidationError]
  type Annotation = WithPointer[ValidationAnnotation]

  def invalid(errors: Seq[Error]): ValidationOutput = ValidationOutput(errors)

  def valid(annotation: ValidationAnnotation, pointer: Pointer = Pointer.empty): ValidationOutput =
    ValidationOutput(Seq(), Seq(WithPointer(annotation, pointer)))

  def invalid(error: ValidationError, pointer: Pointer = Pointer.empty): ValidationOutput = invalid(
    Seq(WithPointer(error, pointer))
  )

  def add(o1: ValidationOutput, o2: ValidationOutput): ValidationOutput =
    ValidationOutput(o1.errors ++ o2.errors, o1.annotations ++ o2.annotations)
}
