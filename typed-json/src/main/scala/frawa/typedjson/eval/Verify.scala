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
import frawa.typedjson.parser.Value.{NullValue, *}
import frawa.typedjson.pointer.Pointer
import frawa.typedjson.validation.*
import frawa.typedjson.validation.ValidationProcessing.EvalFun

import scala.reflect.TypeTest

class Verify[R[_], O](using TheResultMonad[R, O], OutputOps[O]):

  import Eval.Fun

  val nullTypeMismatch: TypeMismatch[NullValue.type] = TypeMismatch("null")
  val booleanTypeMismatch: TypeMismatch[BoolValue]   = TypeMismatch("boolean")
  val stringTypeMismatch: TypeMismatch[StringValue]  = TypeMismatch("string")
  val numberTypeMismatch: TypeMismatch[NumberValue]  = TypeMismatch("number")
  val arrayTypeMismatch: TypeMismatch[ArrayValue]    = TypeMismatch("array")
  val objectTypeMismatch: TypeMismatch[ObjectValue]  = TypeMismatch("object")

  private val ops   = summon[OutputOps[O]]
  private val monad = summon[TheResultMonad[R, O]]

  // utils

  private def funUnit(f: Fun[O]): Fun[R[O]] =
    value => monad.unit(f(value))

  private def funUnit2(f: Fun[Option[O]]): Fun[R[O]] =
    value => monad.unit(f(value).getOrElse(ops.valid(value.pointer)))

  private def funMap[A, B](fun: Fun[R[A]])(f: (A, Pointer) => B): Fun[R[B]] =
    value => fun(value).map(a => f(a, value.pointer))

  private def verifyAllOf(os: Seq[O], pointer: Pointer): O =
    ops.all(os, pointer)

  private def verifyNot(o: O, pointer: Pointer): O =
    o.not(pointer)

  private def verifyOneOf(os: Seq[O], pointer: Pointer): O =
    val count = os.count(_.isValid)
    if count == 1 then ops.valid(pointer)
    else if count == 0 then ops.all(os, pointer) // TODO new error NoneOf?
    else ops.invalid(NotOneOf(count), pointer)

  private def verifyAnyOf(os: Seq[O], pointer: Pointer): O =
    val valid = os.exists(_.isValid)
    if valid then ops.valid(pointer)
    else ops.all(os, pointer) // TODO new error NoneOf?

  // verifys

  def verifyType[T <: Value](error: TypeMismatch[T])(using TypeTest[Value, T]): Fun[R[O]] =
    funUnit { value =>
      value.value match
        case _: T => ops.valid(value.pointer)
        case _    => ops.invalid(error, value.pointer)
    }

  def verifyTrivial(valid: Boolean): Fun[R[O]] =
    funUnit { value =>
      if valid then ops.valid(value.pointer)
      else ops.invalid(FalseSchemaReason(), value.pointer)
    }

  def verifyAllOf(fun: Fun[R[Seq[O]]]): Fun[R[O]] = funMap(fun)(verifyAllOf)
  def verifyNot(fun: Fun[R[O]]): Fun[R[O]]        = funMap(fun)(verifyNot)
  def verifyUnion(fs: Fun[R[Seq[O]]]): Fun[R[O]]  = verifyOneOf(fs)
  def verifyOneOf(fun: Fun[R[Seq[O]]]): Fun[R[O]] = funMap(fun)(verifyOneOf)
  def verifyAnyOf(fun: Fun[R[Seq[O]]]): Fun[R[O]] = funMap(fun)(verifyAnyOf)

  def verifyArrayItems(items: Option[Fun[R[O]]], prefixItems: Seq[Fun[R[O]]]): Fun[R[O]] = value =>
    items
      .zip(Value.asArray(value.value))
      .map { (f, vs) =>
        vs.zipWithIndex.map((v, i) => f(WithPointer(v, value.pointer / i)))
      }
      .map(ros => FP.Util.sequence(ros).map(os => ops.all(os, value.pointer)))
      .getOrElse { monad.unit(ops.valid(value.pointer)) }

  def verfyObjectProperties(
      properties: Map[String, Fun[R[O]]],
      patternProperties: Map[String, Fun[R[O]]],
      additionalProperties: Option[Fun[R[O]]]
  ): Fun[R[O]] = value =>
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
      .map(ros => FP.Util.sequence(ros).map(os => ops.all(os, value.pointer)))
      .getOrElse(monad.unit(ops.valid(value.pointer)))

  def verifyObjectRequired(names: Seq[String]): Fun[R[O]] =
    funUnit2 { value =>
      Value
        .asObject(value.value)
        .map { vs =>
          val missingNames = names.filter(!vs.contains(_))
          if missingNames.isEmpty then ops.valid(value.pointer)
          else ops.invalid(MissingRequiredProperties(missingNames), value.pointer)
        }
    }

  def verfyIfThenElse(fIf: Option[Fun[R[O]]], fThen: Option[Fun[R[O]]], fElse: Option[Fun[R[O]]]): Fun[R[O]] = value =>
    fIf
      .map { fIf =>
        val condition = fIf(value)
        condition.map(_.isValid).flatMap { isValid =>
          val fully =
            if isValid then
              fThen
                .map(_(value))
                .map(fThen => FP.Util.sequence(Seq(condition, fThen)).map(os => ops.all(os, value.pointer)))
            else fElse.map(fElse => fElse(value))
          fully.getOrElse(monad.unit(ops.valid(value.pointer)))
        }
      }
      .getOrElse(monad.unit(ops.valid(value.pointer)))

  def verifyEnum(vs: Seq[Value]): Fun[R[O]] =
    funUnit { value =>
      if vs.contains(value.value) then ops.valid(value.pointer)
      else ops.invalid(NotInEnum(vs), value.pointer)
    }

  def verifyMinItems(min: BigDecimal): Fun[R[O]] =
    funUnit2 { value =>
      Value
        .asArray(value.value)
        .map { vs =>
          if min <= vs.length then ops.valid(value.pointer)
          else ops.invalid(MinItemsMismatch(min), value.pointer)
        }
    }

  def verifyUniqueItems(unique: Boolean): Fun[R[O]] =
    funUnit2 { value =>
      Value
        .asArray(value.value)
        .map { vs =>
          if !unique || vs.distinct.length == vs.length then ops.valid(value.pointer)
          else ops.invalid(ItemsNotUnique(), value.pointer)
        }
    }

  def verifyMinimum(min: BigDecimal, exclude: Boolean): Fun[R[O]] =
    verifyNumberValue(MinimumMismatch(min, exclude)) { v =>
      if exclude then min < v else min <= v
    }

  private def verifyNumberValue(error: => ValidationError)(validate: BigDecimal => Boolean): Fun[R[O]] =
    funUnit2 { value =>
      Value
        .asNumber(value.value)
        .map { v =>
          if validate(v) then ops.valid(value.pointer)
          else ops.invalid(error, value.pointer)
        }
    }

  def verifyPattern(pattern: String): Fun[R[O]] =
    val r = pattern.r
    funUnit2 { value =>
      Value
        .asString(value.value)
        .map { v =>
          if r.findFirstIn(v).isDefined then ops.valid(value.pointer)
          else ops.invalid(PatternMismatch(pattern), value.pointer)
        }
    }

  def verifyPropertyNames(f: Fun[R[O]]): Fun[R[O]] = value =>
    Value
      .asObject(value.value)
      .map { vs =>
        val names = vs.keySet
        val ros = names.map { name =>
          (name, f(WithPointer(StringValue(name), value.pointer / name)))
        }.toSeq
        // val validNames = os.filter(_._2.isValid).map(_._1).toSet
        // TODO annotation EvaluatedProperties() with validNames
        FP.Util.sequence(ros.map(_._2)).map(os => ops.all(os, value.pointer))
      }
      .getOrElse(monad.unit(ops.valid(value.pointer)))

  def verifyFormat(format: String): Fun[R[O]] =
    funUnit2 { value =>
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
    // TODO
    // ops.invalid(UnknownFormat(format), value.pointer)
    }

  def verifyInteger(): Fun[R[O]] =
    funUnit2 { value =>
      Value
        .asNumber(value.value)
        .filterNot(_.isWhole)
        .map { _ =>
          ops.invalid(TypeMismatch[NumberValue]("integer"), value.pointer)
        }
    }
