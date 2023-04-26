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
import frawa.typedjson.keywords.EvaluatedIndices

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

  private def verifyNumberValue(error: => ValidationError)(validate: BigDecimal => Boolean): Fun[R[O]] =
    funUnit2 { value =>
      Value
        .asNumber(value.value)
        .map { v =>
          if validate(v) then ops.valid(value.pointer)
          else ops.invalid(error, value.pointer)
        }
    }

  private def verifyStringValue(error: => ValidationError)(validate: String => Boolean): Fun[R[O]] =
    funUnit2 { value =>
      Value
        .asString(value.value)
        .map { v =>
          if validate(v) then ops.valid(value.pointer)
          else ops.invalid(error, value.pointer)
        }
    }

  private def verifyArrayValue(error: => ValidationError)(validate: Seq[Value] => Boolean): Fun[R[O]] =
    funUnit2 { value =>
      Value
        .asArray(value.value)
        .map { v =>
          if validate(v) then ops.valid(value.pointer)
          else ops.invalid(error, value.pointer)
        }
    }

  private def verifyObjectValue(error: => ValidationError)(validate: Map[String, Value] => Boolean): Fun[R[O]] =
    funUnit2 { value =>
      Value
        .asObject(value.value)
        .map { v =>
          if validate(v) then ops.valid(value.pointer)
          else ops.invalid(error, value.pointer)
        }
    }

  private def countCharPoints(text: String): Int =
    var i     = 0
    var count = 0
    while i < text.length() do
      i += Character.charCount(text.codePointAt(i))
      count += 1
    count

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
    Value
      .asArray(value.value)
      .map { vs =>
        val indexed = vs.zipWithIndex
          .map { (v, index) =>
            WithPointer(v, value.pointer / index)
          }
        val ros1 = prefixItems.zip(indexed).map { (f, v) =>
          f(v)
        }
        val ros2 = items
          .map { items =>
            indexed.drop(prefixItems.size).map(items)
          }
          .getOrElse(Seq())
        ros1 ++ ros2
      }
      .map { ros =>
        FP.Util.sequence(ros).map { os =>
          val validIndices = os.zipWithIndex
            .filter(_._1.isValid)
            .map(_._2)
          ops.all(os, value.pointer).withAnnotation(EvaluatedIndices(validIndices))
        }
      }
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
    verifyArrayValue(MinItemsMismatch(min)) { vs =>
      min <= vs.length
    }

  def verifyMaxItems(max: BigDecimal): Fun[R[O]] =
    verifyArrayValue(MaxItemsMismatch(max)) { vs =>
      max >= vs.length
    }

  def verifyUniqueItems(unique: Boolean): Fun[R[O]] =
    verifyArrayValue(ItemsNotUnique()) { vs =>
      !unique || vs.distinct.length == vs.length
    }

  def verifyMinimum(min: BigDecimal, exclude: Boolean): Fun[R[O]] =
    verifyNumberValue(MinimumMismatch(min, exclude)) { v =>
      if exclude then min < v else min <= v
    }

  def verifyPattern(pattern: String): Fun[R[O]] =
    val r = pattern.r
    verifyStringValue(PatternMismatch(pattern)) { v =>
      r.findFirstIn(v).isDefined
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
    verifyNumberValue(TypeMismatch[NumberValue]("integer")) { v =>
      v.isWhole
    }

  def verifyMultiple(n: BigDecimal): Fun[R[O]] =
    verifyNumberValue(NotMultipleOf(n)) { v =>
      (v / n).isValidInt
    }

  def verifyMaxLength(max: BigDecimal): Fun[R[O]] =
    verifyStringValue(MaxLengthMismatch(max)) { v =>
      countCharPoints(v) <= max
    }

  def verifyMinLength(min: BigDecimal): Fun[R[O]] =
    verifyStringValue(MinLengthMismatch(min)) { v =>
      countCharPoints(v) >= min
    }

  def verifyMaxProperties(max: BigDecimal): Fun[R[O]] =
    verifyObjectValue(MaxPropertiesMismatch(max)) { v =>
      max >= v.keySet.size
    }

  def verifyMinProperties(min: BigDecimal): Fun[R[O]] =
    verifyObjectValue(MinPropertiesMismatch(min)) { v =>
      min <= v.keySet.size
    }

  def verifyDependentRequired(required: Map[String, Seq[String]]): Fun[R[O]] = value =>
    Value
      .asObject(value.value)
      .map { v =>
        val missing = required
          .flatMap { case (property, required) =>
            v
              .get(property)
              .map { _ => required.filterNot(v.contains) }
              .filterNot(_.isEmpty)
              .map(property -> _)
          }
        missing
      }
      .filterNot(_.isEmpty)
      .map { missing =>
        monad.unit(ops.invalid(DependentRequiredMissing(missing), value.pointer))
      }
      .getOrElse(monad.unit(ops.valid(value.pointer)))

  def verifyDependentSchemas(
      schemas: Map[String, Fun[R[O]]]
  ): Fun[R[O]] = value =>
    Value
      .asObject(value.value)
      .map { vs =>
        val required = vs.keySet.flatMap(schemas.get)
        required.map(_(value)).toSeq
      }
      .map(ros => FP.Util.sequence(ros).map(os => ops.all(os, value.pointer)))
      .getOrElse(monad.unit(ops.valid(value.pointer)))

  def verifyContains(
      schema: Option[Fun[R[O]]],
      min: Option[Int],
      max: Option[Int]
  ): Fun[R[O]] = value =>
    Value
      .asArray(value.value)
      .flatMap { vs =>
        val indexed = vs.zipWithIndex
          .map { (v, index) =>
            WithPointer(v, value.pointer / index)
          }
        schema.map(indexed.map(_)).map { ros =>
          FP.Util.sequence(ros).map { os =>
            val count = os.count(_.isValid)
            val o =
              if min.getOrElse(1) <= count && !max.exists(count > _) then ops.valid(value.pointer)
              else ops.invalid(NotContains(count), value.pointer)
            val validIndices = os.zipWithIndex
              .filter(_._1.isValid)
              .map(_._2)
            o.withAnnotation(EvaluatedIndices(validIndices))
          }
        }
      }
      .getOrElse(monad.unit(ops.valid(value.pointer)))

  def verifyUnevaluatedItems(
      pushed: Seq[Fun[R[O]]],
      unevaluated: Fun[R[O]]
  ): Fun[R[O]] = value =>
    val ros = pushed.map(_(value))
    Value
      .asArray(value.value)
      .map { vs =>
        val seqEvaluatedIndices = ros
          .map { ro =>
            ro.map { o =>
              val evaluatedIndices = o.annotation
                .map {
                  case EvaluatedIndices(indices) => indices
                  case _                         => Seq()
                }
                .getOrElse(Seq())
                .toSet
              evaluatedIndices
            }
          }
        val ros1 = FP.Util
          .sequence(seqEvaluatedIndices)
          .map(_.flatten.toSet)
          .flatMap { evaluated =>
            val all       = Seq.range(0, vs.length).toSet
            val remaining = if evaluated.isEmpty then all else all.filterNot(evaluated.contains)
            val remainingIndexed = vs.zipWithIndex
              .filter { (v, index) =>
                remaining.contains(index)
              }
              .map { case (v, index) =>
                WithPointer(v, value.pointer / index)
              }
            val ros = remainingIndexed.map(unevaluated)
            FP.Util.sequence(ros).map(os => ops.all(os, value.pointer))
          }
        ros1
        // FP.Util.sequence(ros).map(os => ops.all(os, value.pointer))
      }
      .getOrElse(FP.Util.sequence(ros).map(os => ops.all(os, value.pointer)))
