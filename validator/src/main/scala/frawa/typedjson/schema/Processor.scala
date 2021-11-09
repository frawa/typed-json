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

import frawa.typedjson.parser.ArrayValue
import frawa.typedjson.parser.BoolValue
import frawa.typedjson.parser.NullValue
import frawa.typedjson.parser.NumberValue
import frawa.typedjson.parser.ObjectValue
import frawa.typedjson.parser.Parser
import frawa.typedjson.parser.StringValue
import frawa.typedjson.parser.Value
import frawa.typedjson.parser.ZioParser

import java.net.URI
import scala.reflect.ClassTag
import scala.util.matching.Regex

case class SchemaValue(value: Value)

object SchemaValue {
  def id(schema: SchemaValue): Option[String] = {
    (Pointer.empty / "$id")(schema.value).flatMap {
      case StringValue(id) => Some(id)
      case _               => None
    }
  }
}

case class Processor[R] private[schema] (private val process: Processor.ProcessFun[R], validation: SchemaQuality) {
  def apply(value: InnerValue): Checked[R] = process(value)
}

object Processor {
  import Checks.CheckWithLocation

  type SchemaErrors  = Checks.SchemaErrors
  type ProcessFun[R] = InnerValue => Checked[R]
  type MergeFun[R]   = Seq[Checked[R]] => ProcessFun[R]

  def apply[R](schema: SchemaValue, lazyResolver: Option[LoadedSchemasResolver.LazyResolver] = None)(
      checker: Checker[R]
  ): Either[SchemaErrors, Processor[R]] = {
    implicit val resolver = LoadedSchemasResolver(schema, lazyResolver)

    val scope = DynamicScope.empty.push(resolver.base)
    for {
      checks <- Checks.parseKeywords(schema, scope)
      processor = Processor(all(checker, checks), SchemaQuality.empty.addIgnoredKeywords(checks.ignoredKeywords))
    } yield (processor)
  }

  private def all[R](checker: Checker[R], checks: Checks): ProcessFun[R] = {
    seq(checks.checks.map(one(checker, _)))
      .andThen(_.add(SchemaQuality.empty.addIgnoredKeywords(checks.ignoredKeywords)))
  }

  private def noop[R]: ProcessFun[R]                                            = _ => Checked.valid[R]
  private def simple[R](checker: Checker[R], check: SimpleCheck): ProcessFun[R] = checker.check(check)
  private def seq[R](ps: Seq[ProcessFun[R]]): ProcessFun[R]                     = value => Checked.merge(ps.map(_(value)))
  private def option[R](p: Option[ProcessFun[R]]): ProcessFun[R] = {
    p.getOrElse(noop)
  }

  private def applyToArray[R](pPrefix: Seq[ProcessFun[R]], pItems: Option[ProcessFun[R]])(
      merge: MergeFun[R]
  ): ProcessFun[R] = { value: InnerValue =>
    value.value match {
      case ArrayValue(vs) => {
        val indexed = vs.zipWithIndex
          .map { case (v, index) =>
            InnerValue(v, value.pointer / index)
          }
        val checkedPrefix = pPrefix.zip(indexed).map { case (p, v) => p(v) }
        val checked       = pItems.map(pItems => indexed.drop(pPrefix.size).map(pItems)).getOrElse(Seq())
        val indices       = Seq.range(0, checkedPrefix.size + checked.size)
        merge(checkedPrefix ++ checked)(value).add(WithPointer(EvaluatedIndices(indices)))
      }
      case _ => Checked.valid
    }
  }

  private def applyToObject[R](
      ps: => PartialFunction[String, () => ProcessFun[R]]
  )(merge: MergeFun[R]): ProcessFun[R] = { value =>
    value.value match {
      case ObjectValue(vs) => {
        val evaluated = vs.view
          .map { case (key, v) =>
            (key, InnerValue(v, value.pointer / key))
          }
          .flatMap { case (key, inner) =>
            val p = ps.lift(key)
            p.map(p => (key, p()(inner)))
          }
          .toSeq
        val checked       = evaluated.map(_._2)
        val evaluatedKeys = EvaluatedProperties(evaluated.filter(_._2.valid).map(_._1).toSet)
        val annotation    = WithPointer(evaluatedKeys, value.pointer)
        merge(checked)(value).add(annotation)
      }
      case _ => Checked.valid
    }
  }

  private def applyToValue[R](ps: Seq[ProcessFun[R]])(merge: MergeFun[R]): ProcessFun[R] = { value =>
    val checked = ps.map(_(value))
    merge(checked)(value)
  }

  private def applyCondition[R](pIf: ProcessFun[R], pThen: ProcessFun[R], pElse: ProcessFun[R])(
      merge: MergeFun[R]
  ): ProcessFun[R] = { value =>
    val ifChecked = pIf(value)
    val checked   = if (ifChecked.valid) Seq(ifChecked, pThen(value)) else Seq(pElse(value))
    merge(checked)(value)
  }

  private def one[R](checker: Checker[R], check: CheckWithLocation): ProcessFun[R] =
    check.value match {
      case c: SimpleCheck  => simple(checker, c)
      case c: NestingCheck => nesting(checker, c)
    }

  private def nesting[R](checker: Checker[R], check: NestingCheck): ProcessFun[R] =
    check match {
      case c: ArrayItemsCheck            => checkArrayItems(checker, c)
      case c: ObjectPropertiesCheck      => checkObjectProperties(checker, c)
      case c: NotCheck                   => checkApplicator(checker, Seq(c.checks))(checker.nested(check))
      case c: AllOfCheck                 => checkApplicator(checker, c.checks)(checker.nested(check))
      case c: AnyOfCheck                 => checkApplicator(checker, c.checks)(checker.nested(check))
      case c: OneOfCheck                 => checkApplicator(checker, c.checks)(checker.nested(check))
      case c: IfThenElseCheck            => checkIfThenElse(checker, c)
      case c: PropertyNamesCheck         => checkPropertyNames(checker, c)
      case c: LazyResolveCheck           => checkLazyResolve(checker, c)
      case c: DependentSchemasCheck      => checkDependentSchemas(checker, c)
      case c: ContainsCheck              => checkContains(checker, c)
      case c: UnevaluatedItemsCheck      => checkUnevaluated(checker, c)
      case c: UnevaluatedPropertiesCheck => checkUnevaluated(checker, c)
    }

  private def checkArrayItems[R](checker: Checker[R], check: ArrayItemsCheck): ProcessFun[R] = {
    val pPrefix = check.prefixItems.map(all(checker, _))
    val pItems  = check.items.map(all(checker, _))
    val merge   = checker.nested(check)
    applyToArray(pPrefix, pItems)(merge)
  }

  private def checkObjectProperties[R](checker: Checker[R], check: ObjectPropertiesCheck): ProcessFun[R] = {
    val psProperties = check.properties.map { case (key, checks) =>
      val partial: PartialFunction[String, () => ProcessFun[R]] = {
        case k if k == key => () => all(checker, checks)
      }
      partial
    }.toSeq

    val psPatterns = check.patternProperties.map { case (regex, checks) =>
      val r = regex.r
      val partial: PartialFunction[String, () => ProcessFun[R]] = {
        case k if r.findFirstIn(k).isDefined => () => all(checker, checks)
      }
      partial
    }.toSeq

    val psBoth = psProperties ++ psPatterns

    val psAll: PartialFunction[String, () => ProcessFun[R]] = {
      case k if psBoth.exists(_.isDefinedAt(k)) =>
        () => seq(psBoth.map(_.lift).flatMap { p => p(k).map(_()) })
    }

    val psAdditional = check.additionalProperties.map { checks =>
      val partial: PartialFunction[String, () => ProcessFun[R]] = { k => () => all(checker, checks) }
      partial
    }

    val ps    = psAdditional.map(psAll.orElse(_)).getOrElse(psAll)
    val merge = checker.nested(check)
    applyToObject(ps)(merge)
  }

  private def checkApplicator[R](checker: Checker[R], checks: Seq[Checks])(merge: MergeFun[R]): ProcessFun[R] = {
    val p = checks.map(all(checker, (_)))
    applyToValue(p)(merge)
  }

  private def checkIfThenElse[R](checker: Checker[R], check: IfThenElseCheck): ProcessFun[R] = {
    check.ifChecks
      .map(ifChecks =>
        applyCondition(
          all(checker, ifChecks),
          option(check.thenChecks.map(all(checker, _))),
          option(check.elseChecks.map(all(checker, _)))
        )(checker.nested(check))
      )
      .getOrElse(noop)
  }

  private def checkPropertyNames[R](checker: Checker[R], check: PropertyNamesCheck): ProcessFun[R] = { value =>
    value.value match {
      case ObjectValue(vs) => {
        val p     = all(checker, check.checks)
        val merge = checker.nested(check)
        val names = vs.keySet
        val checked = names.map { name =>
          (name, p(InnerValue(StringValue(name), value.pointer / name)))
        }.toSeq
        val validNames = checked.filter(_._2.valid).map(_._1).toSet
        val annotation = WithPointer(EvaluatedProperties(validNames), value.pointer)
        merge(checked.map(_._2))(value).add(annotation)
      }
      case _ => Checked.valid
    }
  }

  private def checkLazyResolve[R](checker: Checker[R], check: LazyResolveCheck): ProcessFun[R] = {
    check.resolve() match {
      case Right(checks) => all(checker, checks)
      case Left(errors)  => _ => Checked.invalid.add(SchemaQuality.empty.addErrors(errors))
    }
  }

  private def checkDependentSchemas[R](checker: Checker[R], check: DependentSchemasCheck): ProcessFun[R] = { value =>
    value.value match {
      case ObjectValue(v) =>
        val ps      = v.keySet.flatMap(check.checks.get(_)).map(all(checker, _)).toSeq
        val merge   = checker.nested(check)
        val checked = ps.map(_(value))
        merge(checked)(value)
      case _ => Checked.valid
    }
  }

  private def checkContains[R](checker: Checker[R], check: ContainsCheck): ProcessFun[R] = { value =>
    value.value match {
      case ArrayValue(vs) =>
        check.schema
          .map { schema =>
            val p     = all(checker, schema)
            val merge = checker.nested(check)
            val indexed = vs.zipWithIndex
              .map { case (v, index) =>
                InnerValue(v, value.pointer / index)
              }
            val checked      = indexed.map(p(_))
            val validIndices = checked.zipWithIndex.filter(_._1.valid).map(_._2)
            merge(checked)(value).add(WithPointer(EvaluatedIndices(validIndices), value.pointer))
          }
          .getOrElse(Checked.valid)
      case _ => Checked.valid
    }
  }

  private def checkUnevaluated[R](checker: Checker[R], check: UnevaluatedItemsCheck): ProcessFun[R] = { value =>
    val p       = all(checker, check.pushedChecks)
    val checked = p(value)
    value.value match {
      case ArrayValue(vs) =>
        val evaluated = checked.annotations
          .filter(_.pointer == Pointer.empty)
          .flatMap {
            case WithPointer(EvaluatedIndices(indices), _) => indices
            case _                                         => Seq()
          }
          .toSet
        val pUnevaluated = all(checker, check.unevaluated)
        val indexed = vs.zipWithIndex
          .map { case (v, index) =>
            (index, InnerValue(v, value.pointer / index))
          }
        val checkedUnevaluated = indexed
          .filterNot { case (index, _) => evaluated.contains(index) }
          .map(_._2)
          .map(pUnevaluated)
        val merge      = checker.nested(check)
        val allIndices = Seq.range(0, vs.size)
        merge(Seq(checked) ++ checkedUnevaluated)(value).add(WithPointer(EvaluatedIndices(allIndices), value.pointer))
      case _ => checked
    }
  }

  private def checkUnevaluated[R](checker: Checker[R], check: UnevaluatedPropertiesCheck): ProcessFun[R] = { value =>
    val p       = all(checker, check.pushedChecks)
    val checked = p(value)
    value.value match {
      case ObjectValue(vs) =>
        val evaluated = checked.annotations
          .filter(_.pointer == value.pointer)
          .flatMap {
            case WithPointer(EvaluatedProperties(properties), _) => properties
            case _                                               => Seq()
          }
          .toSet
        val pUnevaluated = all(checker, check.unevaluated)
        val checkedUnevaluated = vs
          .filterNot { case (prop, _) => evaluated.contains(prop) }
          .map { case (prop, v) =>
            pUnevaluated(InnerValue(v, value.pointer / prop))
          }
          .toSeq
        // println("FW", value.pointer, evaluated, checked.annotations, checkedUnevaluated.size)
        val merge         = checker.nested(check)
        val allProperties = vs.keySet
        merge(Seq(checked) ++ checkedUnevaluated)(value)
          .add(WithPointer(EvaluatedProperties(allProperties), value.pointer))
      case _ => checked
    }
  }
}
