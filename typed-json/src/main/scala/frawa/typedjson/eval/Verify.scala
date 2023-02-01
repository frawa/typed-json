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

package frawa.typedjson.eval

import frawa.typedjson.keywords.Keyword
import frawa.typedjson.parser.Value
import frawa.typedjson.validation.TypeMismatch
import scala.reflect.TypeTest
import frawa.typedjson.parser.Value._
import frawa.typedjson.pointer.Pointer
import frawa.typedjson.validation.FalseSchemaReason
import frawa.typedjson.keywords.WithPointer
import frawa.typedjson.validation.MissingRequiredProperties

class Verify[O: OutputOps]:
  import Eval.Fun

  val nullTypeMismatch    = TypeMismatch[NullValue.type]("null")
  val booleanTypeMismatch = TypeMismatch[BoolValue]("boolean")
  val stringTypeMismatch  = TypeMismatch[StringValue]("string")
  val numberTypeMismatch  = TypeMismatch[NumberValue]("number")
  val arrayTypeMismatch   = TypeMismatch[ArrayValue]("array")
  val objectTypeMismatch  = TypeMismatch[ObjectValue]("object")

  private val ops = summon[OutputOps[O]]

  def verifyType[T <: Value](error: TypeMismatch[T])(using TypeTest[Value, T]): Fun[O] = value =>
    value.value match
      case _: T => ops.valid
      case _    => ops.invalid(error, value.pointer)

  def verifyTrivial(valid: Boolean): Fun[O] = value =>
    if valid then ops.valid
    else ops.invalid(FalseSchemaReason(), value.pointer)

  def verifyNot(f: Fun[O]): Fun[O]         = Eval.map(f)(_.not)
  def verifyUnion(fs: Seq[Fun[O]]): Fun[O] = ???
  def verifyAll(fs: Seq[Fun[O]]): Fun[O]   = value => ops.all(fs.map(_(value)))

  def verfyArrayItems(items: Option[Fun[O]], prefixItems: Seq[Fun[O]]): Fun[O] = value =>
    items
      .zip(Value.asArray(value.value))
      .map { (f, vs) =>
        vs.zipWithIndex.map((v, i) => f(WithPointer(v, value.pointer / i)))
      }
      .map(os => ops.all(os))
      .getOrElse(ops.valid)

  def verfyObjectProperties(
      properties: Map[String, Fun[O]],
      patternProperties: Map[String, Fun[O]],
      additionalProperties: Option[Fun[O]]
  ): Fun[O] = value =>
    Value
      .asObject(value.value)
      .map { vs =>
        properties.flatMap { (p, f) =>
          vs.get(p).map(v => f(WithPointer(v, value.pointer / p)))
        }.toSeq
      }
      .map(os => ops.all(os))
      .getOrElse(ops.valid)

  def verifyObjectRequired(names: Seq[String]): Fun[O] = value =>
    Value
      .asObject(value.value)
      .map { vs =>
        val missingNames = names.filter(!vs.contains(_))
        if missingNames.isEmpty then ops.valid
        else ops.invalid(MissingRequiredProperties(missingNames), value.pointer)
      }
      .getOrElse(ops.valid)

  def verifyAllOf(fs: Seq[Fun[O]]): Fun[O] = value => ops.all(fs.map(_(value)))
  def verifyAnyOf(fs: Seq[Fun[O]]): Fun[O] = value => ops.any(fs.map(_(value)))
