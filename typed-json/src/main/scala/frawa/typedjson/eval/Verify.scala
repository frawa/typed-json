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

import frawa.typedjson.keywords.{Keyword, WithPointer}
import frawa.typedjson.parser.Value
import frawa.typedjson.parser.Value.*
import frawa.typedjson.pointer.Pointer
import frawa.typedjson.validation.*
import frawa.typedjson.validation.ValidationProcessing.EvalFun

import scala.reflect.TypeTest

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
      case _: T => ops.valid(value.pointer)
      case _    => ops.invalid(error, value.pointer)

  def verifyTrivial(valid: Boolean): Fun[O] = value =>
    if valid then ops.valid(value.pointer)
    else ops.invalid(FalseSchemaReason(), value.pointer)

  def verifyNot(f: Fun[O]): Fun[O]         = value => f(value).not(value.pointer)
  def verifyUnion(fs: Seq[Fun[O]]): Fun[O] = verifyOneOf(fs)
  def verifyAll(fs: Seq[Fun[O]]): Fun[O]   = value => ops.all(fs.map(_(value)), value.pointer)

  def verfyArrayItems(items: Option[Fun[O]], prefixItems: Seq[Fun[O]]): Fun[O] = value =>
    items
      .zip(Value.asArray(value.value))
      .map { (f, vs) =>
        vs.zipWithIndex.map((v, i) => f(WithPointer(v, value.pointer / i)))
      }
      .map(os => ops.all(os, value.pointer))
      .getOrElse(ops.valid(value.pointer))

  def verfyObjectProperties(
      properties: Map[String, Fun[O]],
      patternProperties: Map[String, Fun[O]],
      additionalProperties: Option[Fun[O]]
  ): Fun[O] = value =>
    Value
      .asObject(value.value)
      .map { vs =>
        vs.flatMap { (p, v) =>
          val f = properties
            .get(p)
            .orElse {
              patternProperties
                .find { (regex, f) =>
                  val r = regex.r
                  r.findFirstIn(p).isDefined
                }
                .map(_._2)
            }
            .orElse {
              additionalProperties
            }
          f.map(f => f(WithPointer(v, value.pointer / p)))
        }.toSeq
      }
      .map(os => ops.all(os, value.pointer))
      .getOrElse(ops.valid(value.pointer))

  def verifyObjectRequired(names: Seq[String]): Fun[O] = value =>
    Value
      .asObject(value.value)
      .map { vs =>
        val missingNames = names.filter(!vs.contains(_))
        if missingNames.isEmpty then ops.valid(value.pointer)
        else ops.invalid(MissingRequiredProperties(missingNames), value.pointer)
      }
      .getOrElse(ops.valid(value.pointer))

  def verifyAllOf(fs: Seq[Fun[O]]): Fun[O] = value => ops.all(fs.map(_(value)), value.pointer)
  def verifyAnyOf(fs: Seq[Fun[O]]): Fun[O] = value =>
    val os    = fs.map(_(value))
    val valid = os.exists(_.isValid)
    if valid then ops.valid(value.pointer)
    else ops.all(os, value.pointer) // TODO new error NoneOf?

  def verifyOneOf(fs: Seq[Fun[O]]): Fun[O] = value =>
    val os    = fs.map(_(value))
    val count = os.count(_.isValid)
    if count == 1 then ops.valid(value.pointer)
    else if count == 0 then ops.all(os, value.pointer) // TODO new error NoneOf?
    else ops.invalid(NotOneOf(count), value.pointer)

  def verfyIfThenElse(fIf: Option[Fun[O]], fThen: Option[Fun[O]], fElse: Option[Fun[O]]): Fun[O] = value =>
    fIf
      .flatMap { fIf =>
        val condition = fIf(value)
        if condition.isValid then fThen.map(fThen => ops.all(Seq(condition, fThen(value)), value.pointer))
        else fElse.map(fElse => fElse(value))
      }
      .getOrElse(ops.valid(value.pointer))

  def verifyEnum(vs: Seq[Value]): Fun[O] = value =>
    if vs.contains(value.value) then ops.valid(value.pointer)
    else ops.invalid(NotInEnum(vs), value.pointer)

  def verifyMinItems(min: BigDecimal): Fun[O] = value =>
    Value
      .asArray(value.value)
      .map { vs =>
        if min <= vs.length then ops.valid(value.pointer)
        else ops.invalid(MinItemsMismatch(min), value.pointer)
      }
      .getOrElse(ops.valid(value.pointer))

  def verifyUniqueItems(unique: Boolean): Fun[O] = value =>
    Value
      .asArray(value.value)
      .map { vs =>
        if !unique || vs.distinct.length == vs.length then ops.valid(value.pointer)
        else ops.invalid(ItemsNotUnique(), value.pointer)
      }
      .getOrElse(ops.valid(value.pointer))

  def verifyMinimum(min: BigDecimal, exclude: Boolean): Fun[O] =
    verifyNumberValue(MinimumMismatch(min, exclude)) { v =>
      if exclude then min < v else min <= v
    }

  private def verifyNumberValue(error: => ValidationError)(validate: BigDecimal => Boolean): Fun[O] = value =>
    Value
      .asNumber(value.value)
      .map { v =>
        if validate(v) then ops.valid(value.pointer)
        else ops.invalid(error, value.pointer)
      }
      .getOrElse(ops.valid(value.pointer));

  def verifyPattern(pattern: String): Fun[O] =
    val r = pattern.r
    value =>
      Value
        .asString(value.value)
        .map { v =>
          if r.findFirstIn(v).isDefined then ops.valid(value.pointer)
          else ops.invalid(PatternMismatch(pattern), value.pointer)
        }
        .getOrElse(ops.valid(value.pointer))

  def verifyPropertyNames(f: Fun[O]): Fun[O] = value =>
    Value
      .asObject(value.value)
      .map { vs =>
        val names = vs.keySet
        val os = names.map { name =>
          (name, f(WithPointer(StringValue(name), value.pointer / name)))
        }.toSeq
        // val validNames = os.filter(_._2.isValid).map(_._1).toSet
        // TODO annotation EvaluatedProperties() with validNames
        ops.all(os.map(_._2), value.pointer)
      }
      .getOrElse(ops.valid(value.pointer))

  def verifyFormat(format: String): Fun[O] = value =>
    Formats
      .hasFormat(format)
      .map { pred =>
        Value
          .asString(value.value)
          .filterNot(pred)
          .map { _ =>
            ops.invalid(FormatMismatch(format), value.pointer)
          }
          .getOrElse(ops.valid(value.pointer))
      }
      .getOrElse(ops.valid(value.pointer))
    // TODO
//      .getOrElse(ops.invalid(UnknownFormat(format), value.pointer))

  def verifyInteger(): Fun[O] = value =>
    Value
      .asNumber(value.value)
      .filterNot(_.isWhole)
      .map { _ =>
        ops.invalid(TypeMismatch[NumberValue]("integer"), value.pointer)
      }
      .getOrElse(ops.valid(value.pointer))
