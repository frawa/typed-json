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

package frawa.typedjson.processor

import frawa.typedjson
import frawa.typedjson.parser.ArrayValue
import frawa.typedjson.parser.ObjectValue
import frawa.typedjson.parser.StringValue
import frawa.typedjson.processor

case class Processor[R] private[processor] (private val process: Processor.ProcessFun[R], problems: SchemaProblems) {
  def apply(value: InnerValue): Result[R] = process(value)
}

object Processor {
  import Keywords.KeywordWithLocation

  type SchemaErrors  = Keywords.SchemaErrors
  type ProcessFun[R] = InnerValue => Result[R]
  type MergeFun[R]   = Seq[Result[R]] => ProcessFun[R]

  def apply[R](schema: SchemaValue, lazyResolver: Option[LoadedSchemasResolver.LazyResolver] = None)(
      checker: Checker[R]
  ): Either[SchemaErrors, Processor[R]] = {
    implicit val resolver: LoadedSchemasResolver = LoadedSchemasResolver(schema, lazyResolver)

    val scope = DynamicScope.empty.push(resolver.base)
    for {
      keywords <- Keywords.parseKeywords(schema, scope)
      processor = Processor(all(checker, keywords), SchemaProblems.empty.addIgnoredKeywords(keywords.ignored))
    } yield processor
  }

  private def all[R](checker: Checker[R], keywords: Keywords): ProcessFun[R] = {
    seq(keywords.keywords.map(one(checker, _)))
      .andThen(_.add(SchemaProblems.empty.addIgnoredKeywords(keywords.ignored)))
  }

  private def noop[R]: ProcessFun[R]                                              = _ => Result.valid[R]
  private def simple[R](checker: Checker[R], check: SimpleKeyword): ProcessFun[R] = checker.simple(check)
  private def seq[R](ps: Seq[ProcessFun[R]]): ProcessFun[R] = value => Result.merge(ps.map(_(value)))
  private def option[R](p: Option[ProcessFun[R]]): ProcessFun[R] = {
    p.getOrElse(noop)
  }

  private def applyToArray[R](pPrefix: Seq[ProcessFun[R]], pItems: Option[ProcessFun[R]])(
      merge: MergeFun[R]
  ): ProcessFun[R] = { value: InnerValue =>
    value.value match {
      case ArrayValue(vs) =>
        val indexed = vs.zipWithIndex
          .map { case (v, index) =>
            InnerValue(v, value.pointer / index)
          }
        val checkedPrefix = pPrefix.zip(indexed).map { case (p, v) => p(v) }
        val checked       = pItems.map(pItems => indexed.drop(pPrefix.size).map(pItems)).getOrElse(Seq())
        val indices       = Seq.range(0, checkedPrefix.size + checked.size)
        mergeAll(merge, checkedPrefix ++ checked, value).add(WithPointer(EvaluatedIndices(indices)))
      case _ => Result.valid
    }
  }

  // TODO return a ProcessFun[R]?
  private def mergeAll[R](merge: MergeFun[R], cs: Seq[Result[R]], value: InnerValue): Result[R] = {
    merge(cs)(value).add(cs)
  }

  private def applyToObject[R](
      ps: => PartialFunction[String, () => ProcessFun[R]]
  )(merge: MergeFun[R]): ProcessFun[R] = { value =>
    value.value match {
      case ObjectValue(vs) =>
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
        val annotation    = processor.WithPointer(evaluatedKeys, value.pointer)
        mergeAll(merge, checked, value).add(annotation)
      case _ => Result.valid
    }
  }

  private def applyToValue[R](ps: Seq[ProcessFun[R]])(merge: MergeFun[R]): ProcessFun[R] = { value =>
    val checked = ps.map(_(value))
    mergeAll(merge, checked, value)
  }

  private def applyCondition[R](pIf: ProcessFun[R], pThen: ProcessFun[R], pElse: ProcessFun[R])(
      merge: MergeFun[R]
  ): ProcessFun[R] = { value =>
    val ifChecked = pIf(value)
    val checked   = if (ifChecked.valid) Seq(ifChecked, pThen(value)) else Seq(pElse(value))
    mergeAll(merge, checked, value)
  }

  private def one[R](checker: Checker[R], check: KeywordWithLocation): ProcessFun[R] = {
    check.value match {
      case c: SimpleKeyword  => value => simple(checker, c)(value)
      case c: NestingKeyword => value => nesting(checker, c)(value)
    }
  }

  private def nesting[R](checker: Checker[R], check: NestingKeyword): ProcessFun[R] =
    check match {
      case c: ArrayItemsKeyword            => checkArrayItems(checker, c)
      case c: ObjectPropertiesKeyword      => checkObjectProperties(checker, c)
      case c: NotKeyword                   => checkApplicator(checker, Seq(c.keywords))(checker.nested(check))
      case c: AllOfKeyword                 => checkApplicator(checker, c.keywords)(checker.nested(check))
      case c: AnyOfKeyword                 => checkApplicator(checker, c.keywords)(checker.nested(check))
      case c: OneOfKeyword                 => checkApplicator(checker, c.keywords)(checker.nested(check))
      case c: UnionTypeKeyword             => checkUnionType(checker, c)(checker.nested(check))
      case c: IfThenElseKeyword            => checkIfThenElse(checker, c)
      case c: PropertyNamesKeyword         => checkPropertyNames(checker, c)
      case c: LazyResolveKeyword           => checkLazyResolve(checker, c)
      case c: DependentSchemasKeyword      => checkDependentSchemas(checker, c)
      case c: ContainsKeyword              => checkContains(checker, c)
      case c: UnevaluatedItemsKeyword      => checkUnevaluated(checker, c)
      case c: UnevaluatedPropertiesKeyword => checkUnevaluated(checker, c)
    }

  private def checkArrayItems[R](checker: Checker[R], check: ArrayItemsKeyword): ProcessFun[R] = {
    val pPrefix = check.prefixItems.map(all(checker, _))
    val pItems  = check.items.map(all(checker, _))
    val merge   = checker.nested(check)
    applyToArray(pPrefix, pItems)(merge)
  }

  private def checkObjectProperties[R](checker: Checker[R], keyword: ObjectPropertiesKeyword): ProcessFun[R] = {
    val psProperties = keyword.properties.map { case (key, keywords) =>
      val partial: PartialFunction[String, () => ProcessFun[R]] = {
        case k if k == key =>
          () => all(checker, keywords)
      }
      partial
    }.toSeq

    val psPatterns = keyword.patternProperties.map { case (regex, keywords) =>
      val r = regex.r
      val partial: PartialFunction[String, () => ProcessFun[R]] = {
        case k if r.findFirstIn(k).isDefined => () => all(checker, keywords)
      }
      partial
    }.toSeq

    val psBoth = psProperties ++ psPatterns

    val psAll: PartialFunction[String, () => ProcessFun[R]] = {
      case k if psBoth.exists(_.isDefinedAt(k)) =>
        () => seq(psBoth.map(_.lift).flatMap { p => p(k).map(_()) })
    }

    val psAdditional = keyword.additionalProperties.map { keywords =>
      val partial: PartialFunction[String, () => ProcessFun[R]] = { _ => () => all(checker, keywords) }
      partial
    }

    val ps    = psAdditional.map(psAll.orElse(_)).getOrElse(psAll)
    val merge = checker.nested(keyword)
    value => {
      applyToObject(ps)(merge)(value)
    }
  }

  private def checkApplicator[R](checker: Checker[R], keywords: Seq[Keywords])(merge: MergeFun[R]): ProcessFun[R] = {
    val p = keywords.map(all(checker, _))
    applyToValue(p)(merge)
  }

  private def checkUnionType[R](checker: Checker[R], keyword: UnionTypeKeyword)(merge: MergeFun[R]): ProcessFun[R] = {
    val p = keyword.keywords.map(one(checker, _))
    applyToValue(p)(merge)
  }

  private def checkIfThenElse[R](checker: Checker[R], keyword: IfThenElseKeyword): ProcessFun[R] = {
    keyword.ifKeywords
      .map(ifChecks =>
        applyCondition(
          all(checker, ifChecks),
          option(keyword.thenKeywords.map(all(checker, _))),
          option(keyword.elseKeywords.map(all(checker, _)))
        )(checker.nested(keyword))
      )
      .getOrElse(noop)
  }

  private def checkPropertyNames[R](checker: Checker[R], keyword: PropertyNamesKeyword): ProcessFun[R] = { value =>
    value.value match {
      case ObjectValue(vs) =>
        val p     = all(checker, keyword.keywords)
        val merge = checker.nested(keyword)
        val names = vs.keySet
        val checked = names.map { name =>
          (name, p(InnerValue(StringValue(name), value.pointer / name)))
        }.toSeq
        val validNames = checked.filter(_._2.valid).map(_._1).toSet
        val annotation = processor.WithPointer(EvaluatedProperties(validNames), value.pointer)
        mergeAll(merge, checked.map(_._2), value).add(annotation)
      case _ => Result.valid
    }
  }

  private def checkLazyResolve[R](checker: Checker[R], keyword: LazyResolveKeyword): ProcessFun[R] = {
    keyword.resolve() match {
      case Right(keywords) => all(checker, keywords)
      case Left(errors) =>
        _ =>
          val problems = SchemaProblems.empty.addErrors(errors)
          Result.invalid.add(problems)
    }
  }

  private def checkDependentSchemas[R](checker: Checker[R], keyword: DependentSchemasKeyword): ProcessFun[R] = {
    value =>
      value.value match {
        case ObjectValue(v) =>
          val ps      = v.keySet.flatMap(keyword.keywords.get).map(all(checker, _)).toSeq
          val merge   = checker.nested(keyword)
          val checked = ps.map(_(value))
          mergeAll(merge, checked, value)
        case _ => Result.valid
      }
  }

  private def checkContains[R](checker: Checker[R], keyword: ContainsKeyword): ProcessFun[R] = { value =>
    value.value match {
      case ArrayValue(vs) =>
        keyword.schema
          .map { schema =>
            val p     = all(checker, schema)
            val merge = checker.nested(keyword)
            val indexed = vs.zipWithIndex
              .map { case (v, index) =>
                InnerValue(v, value.pointer / index)
              }
            val checked      = indexed.map(p(_))
            val validIndices = checked.zipWithIndex.filter(_._1.valid).map(_._2)
            mergeAll(merge, checked, value).add(
              typedjson.processor.WithPointer(EvaluatedIndices(validIndices), value.pointer)
            )
          }
          .getOrElse(Result.valid)
      case _ => Result.valid
    }
  }

  private def checkUnevaluated[R](checker: Checker[R], keyword: UnevaluatedItemsKeyword): ProcessFun[R] = { value =>
    val p       = all(checker, keyword.pushed)
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
        val pUnevaluated = all(checker, keyword.unevaluated)
        val indexed = vs.zipWithIndex
          .map { case (v, index) =>
            (index, InnerValue(v, value.pointer / index))
          }
        val checkedUnevaluated = indexed
          .filterNot { case (index, _) => evaluated.contains(index) }
          .map(_._2)
          .map(pUnevaluated)
        val merge      = checker.nested(keyword)
        val allIndices = Seq.range(0, vs.size)
        mergeAll(merge, Seq(checked) ++ checkedUnevaluated, value)
          .add(processor.WithPointer(EvaluatedIndices(allIndices), value.pointer))
      case _ => checked
    }
  }

  private def checkUnevaluated[R](checker: Checker[R], keyword: UnevaluatedPropertiesKeyword): ProcessFun[R] = {
    value =>
      val p       = all(checker, keyword.pushed)
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
          val pUnevaluated = all(checker, keyword.unevaluated)
          val checkedUnevaluated = vs
            .filterNot { case (prop, _) => evaluated.contains(prop) }
            .map { case (prop, v) =>
              pUnevaluated(InnerValue(v, value.pointer / prop))
            }
            .toSeq
          val merge         = checker.nested(keyword)
          val allProperties = vs.keySet
          mergeAll(merge, Seq(checked) ++ checkedUnevaluated, value)
            .add(processor.WithPointer(EvaluatedProperties(allProperties), value.pointer))
        case _ => checked
      }
  }
}
