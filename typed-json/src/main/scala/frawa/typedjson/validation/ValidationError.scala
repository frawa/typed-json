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

import frawa.typedjson.keywords.*
import frawa.typedjson.parser.*
import frawa.typedjson.parser.Value.*
import frawa.typedjson.pointer.Pointer

import java.net.URI
import java.util.UUID
import java.util.regex.Pattern
import scala.reflect.TypeTest
import scala.util.Try
import frawa.typedjson.output.OutputOps

sealed trait ValidationError
case class SubSchemaFailed()                                            extends ValidationError
case class FalseSchemaReason()                                          extends ValidationError
case class TypeMismatch[T <: Value](expected: String)                   extends ValidationError
case class NotOneOf(valid: Int)                                         extends ValidationError
case class NotInvalid()                                                 extends ValidationError
case class NotInEnum(values: Seq[Value])                                extends ValidationError
case class MissingRequiredProperties(properties: Seq[String])           extends ValidationError
case class AdditionalPropertyInvalid(property: String)                  extends ValidationError
case class PatternMismatch(pattern: String)                             extends ValidationError
case class FormatMismatch(format: String)                               extends ValidationError
case class MinimumMismatch(min: BigDecimal, exclude: Boolean)           extends ValidationError
case class ItemsNotUnique()                                             extends ValidationError
case class NotMultipleOf(n: BigDecimal)                                 extends ValidationError
case class MaximumMismatch(max: BigDecimal, exclude: Boolean)           extends ValidationError
case class MaxLengthMismatch(max: BigDecimal)                           extends ValidationError
case class MinLengthMismatch(min: BigDecimal)                           extends ValidationError
case class MaxItemsMismatch(max: BigDecimal, found: BigDecimal)         extends ValidationError
case class MinItemsMismatch(min: BigDecimal, found: BigDecimal)         extends ValidationError
case class MaxPropertiesMismatch(max: BigDecimal, found: BigDecimal)    extends ValidationError
case class MinPropertiesMismatch(min: BigDecimal, found: BigDecimal)    extends ValidationError
case class DependentRequiredMissing(missing: Map[String, Seq[String]])  extends ValidationError
case class NotContains(valid: Int)                                      extends ValidationError
case class CannotResolve(ref: String, problems: Option[SchemaProblems]) extends ValidationError
case class CannotResolveDynamic(ref: String, scope: DynamicScope, problems: Option[SchemaProblems])
    extends ValidationError

sealed trait ValidationAnnotation                 extends OutputOps.Annotation
case class Format(valid: Boolean, format: String) extends ValidationAnnotation
case class UnknownFormat(format: String)          extends ValidationAnnotation
