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
import frawa.typedjson.parser.{ArrayValue, ObjectValue, StringValue}
import frawa.typedjson.processor

case class Processor[R] private[processor] (private val process: Processor.ProcessFun[R]) {
  def apply(value: InnerValue): Result[R]              = process(value)
  def andThen(f: Result[R] => Result[R]): Processor[R] = this.copy(process = process.andThen(f))
}

object Processor {
  import Keywords.KeywordWithLocation

  // TODO ProcessFun -> EvalFun
  type ProcessFun[R] = InnerValue => Result[R]
  // TODO MergeFun -> CombineFun
  type MergeFun[R] = Seq[Result[R]] => ProcessFun[R]

  def apply[R](keywords: Keywords, processing: Processing[R]): Processor[R] = {
    Processor(all(processing, keywords).andThen(_.addIgnoredKeywords(keywords.ignored, Pointer.empty)))
  }

  private def all[R](processing: Processing[R], keywords: Keywords): ProcessFun[R] = { value =>
    seq(keywords.keywords.map(one(processing, _)))
      .andThen(_.addIgnoredKeywords(keywords.ignored, value.pointer))(value)
  }

  private def noop[R]: ProcessFun[R] = _ => Result.valid[R]
  private def simple[R](processing: Processing[R], keyword: AssertionKeyword): ProcessFun[R] =
    processing.simple(keyword)
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
        val resultPrefix = pPrefix.zip(indexed).map { case (p, v) => p(v) }
        val result       = pItems.map(pItems => indexed.drop(pPrefix.size).map(pItems)).getOrElse(Seq())
        val indices      = Seq.range(0, resultPrefix.size + result.size)
        mergeAll(merge, resultPrefix ++ result, value).add(WithPointer(EvaluatedIndices(indices)))
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
        val result        = evaluated.map(_._2)
        val evaluatedKeys = EvaluatedProperties(evaluated.filter(_._2.valid).map(_._1).toSet)
        val annotation    = processor.WithPointer(evaluatedKeys, value.pointer)
        mergeAll(merge, result, value).add(annotation)
      case _ => Result.valid
    }
  }

  private def applyToValue[R](ps: Seq[ProcessFun[R]])(merge: MergeFun[R]): ProcessFun[R] = { value =>
    val result = ps.map(_(value))
    mergeAll(merge, result, value)
  }

  private def applyCondition[R](pIf: ProcessFun[R], pThen: ProcessFun[R], pElse: ProcessFun[R])(
      merge: MergeFun[R]
  ): ProcessFun[R] = { value =>
    val ifResult = pIf(value)
    val result   = if (ifResult.valid) Seq(ifResult, pThen(value)) else Seq(pElse(value))
    mergeAll(merge, result, value)
  }

  private def one[R](processing: Processing[R], keyword: KeywordWithLocation): ProcessFun[R] = {
    keyword.value match {
      case c: AssertionKeyword  => value => simple(processing, c)(value)
      case c: ApplicatorKeyword => value => nesting(processing, c)(value)
    }
  }

  private def nesting[R](processing: Processing[R], keyword: ApplicatorKeyword): ProcessFun[R] =
    keyword match {
      case c: ArrayItemsKeyword            => checkArrayItems(processing, c)
      case c: ObjectPropertiesKeyword      => checkObjectProperties(processing, c)
      case c: NotKeyword                   => checkApplicator(processing, Seq(c.keywords))(processing.nested(keyword))
      case c: AllOfKeyword                 => checkApplicator(processing, c.keywords)(processing.nested(keyword))
      case c: AnyOfKeyword                 => checkApplicator(processing, c.keywords)(processing.nested(keyword))
      case c: OneOfKeyword                 => checkApplicator(processing, c.keywords)(processing.nested(keyword))
      case c: UnionTypeKeyword             => checkUnionType(processing, c)(processing.nested(keyword))
      case c: IfThenElseKeyword            => checkIfThenElse(processing, c)
      case c: PropertyNamesKeyword         => checkPropertyNames(processing, c)
      case c: LazyParseKeywords            => checkLazyParseKeywords(processing, c)
      case c: DependentSchemasKeyword      => checkDependentSchemas(processing, c)
      case c: ContainsKeyword              => checkContains(processing, c)
      case c: UnevaluatedItemsKeyword      => checkUnevaluated(processing, c)
      case c: UnevaluatedPropertiesKeyword => checkUnevaluated(processing, c)
    }

  private def checkArrayItems[R](processing: Processing[R], keyword: ArrayItemsKeyword): ProcessFun[R] = {
    val pPrefix = keyword.prefixItems.map(all(processing, _))
    val pItems  = keyword.items.map(all(processing, _))
    val merge   = processing.nested(keyword)
    applyToArray(pPrefix, pItems)(merge)
  }

  private def checkObjectProperties[R](processing: Processing[R], keyword: ObjectPropertiesKeyword): ProcessFun[R] = {
    val psProperties = keyword.properties.map { case (key, keywords) =>
      val partial: PartialFunction[String, () => ProcessFun[R]] = {
        case k if k == key =>
          () => all(processing, keywords)
      }
      partial
    }.toSeq

    val psPatterns = keyword.patternProperties.map { case (regex, keywords) =>
      val r = regex.r
      val partial: PartialFunction[String, () => ProcessFun[R]] = {
        case k if r.findFirstIn(k).isDefined => () => all(processing, keywords)
      }
      partial
    }.toSeq

    val psBoth = psProperties ++ psPatterns

    val psAll: PartialFunction[String, () => ProcessFun[R]] = {
      case k if psBoth.exists(_.isDefinedAt(k)) =>
        () => seq(psBoth.map(_.lift).flatMap { p => p(k).map(_()) })
    }

    val psAdditional = keyword.additionalProperties.map { keywords =>
      val partial: PartialFunction[String, () => ProcessFun[R]] = { _ => () => all(processing, keywords) }
      partial
    }

    val ps    = psAdditional.map(psAll.orElse(_)).getOrElse(psAll)
    val merge = processing.nested(keyword)
    value => {
      applyToObject(ps)(merge)(value)
    }
  }

  private def checkApplicator[R](processing: Processing[R], keywords: Seq[Keywords])(
      merge: MergeFun[R]
  ): ProcessFun[R] = {
    val p = keywords.map(all(processing, _))
    applyToValue(p)(merge)
  }

  private def checkUnionType[R](processing: Processing[R], keyword: UnionTypeKeyword)(
      merge: MergeFun[R]
  ): ProcessFun[R] = {
    val p = keyword.keywords.map(one(processing, _))
    applyToValue(p)(merge)
  }

  private def checkIfThenElse[R](processing: Processing[R], keyword: IfThenElseKeyword): ProcessFun[R] = {
    keyword.ifKeywords
      .map(ifChecks =>
        applyCondition(
          all(processing, ifChecks),
          option(keyword.thenKeywords.map(all(processing, _))),
          option(keyword.elseKeywords.map(all(processing, _)))
        )(processing.nested(keyword))
      )
      .getOrElse(noop)
  }

  private def checkPropertyNames[R](processing: Processing[R], keyword: PropertyNamesKeyword): ProcessFun[R] = {
    value =>
      value.value match {
        case ObjectValue(vs) =>
          val p     = all(processing, keyword.keywords)
          val merge = processing.nested(keyword)
          val names = vs.keySet
          val result = names.map { name =>
            (name, p(InnerValue(StringValue(name), value.pointer / name)))
          }.toSeq
          val validNames = result.filter(_._2.valid).map(_._1).toSet
          val annotation = processor.WithPointer(EvaluatedProperties(validNames), value.pointer)
          mergeAll(merge, result.map(_._2), value).add(annotation)
        case _ => Result.valid
      }
  }

  private def checkLazyParseKeywords[R](processing: Processing[R], keyword: LazyParseKeywords): ProcessFun[R] = {
    keyword.parse() match {
      case Right(keywords) => all(processing, keywords)
      case Left(problems)  => _ => Result.invalid.add(problems)
    }
  }

  private def checkDependentSchemas[R](processing: Processing[R], keyword: DependentSchemasKeyword): ProcessFun[R] = {
    value =>
      value.value match {
        case ObjectValue(v) =>
          val ps     = v.keySet.flatMap(keyword.keywords.get).map(all(processing, _)).toSeq
          val merge  = processing.nested(keyword)
          val result = ps.map(_(value))
          mergeAll(merge, result, value)
        case _ => Result.valid
      }
  }

  private def checkContains[R](processing: Processing[R], keyword: ContainsKeyword): ProcessFun[R] = { value =>
    value.value match {
      case ArrayValue(vs) =>
        keyword.schema
          .map { schema =>
            val p     = all(processing, schema)
            val merge = processing.nested(keyword)
            val indexed = vs.zipWithIndex
              .map { case (v, index) =>
                InnerValue(v, value.pointer / index)
              }
            val result       = indexed.map(p(_))
            val validIndices = result.zipWithIndex.filter(_._1.valid).map(_._2)
            mergeAll(merge, result, value).add(
              typedjson.processor.WithPointer(EvaluatedIndices(validIndices), value.pointer)
            )
          }
          .getOrElse(Result.valid)
      case _ => Result.valid
    }
  }

  private def checkUnevaluated[R](processing: Processing[R], keyword: UnevaluatedItemsKeyword): ProcessFun[R] = {
    value =>
      val p      = all(processing, keyword.pushed)
      val result = p(value)
      value.value match {
        case ArrayValue(vs) =>
          val evaluated = result.annotations
            .filter(_.pointer == Pointer.empty)
            .flatMap {
              case WithPointer(EvaluatedIndices(indices), _) => indices
              case _                                         => Seq()
            }
            .toSet
          val pUnevaluated = all(processing, keyword.unevaluated)
          val indexed = vs.zipWithIndex
            .map { case (v, index) =>
              (index, InnerValue(v, value.pointer / index))
            }
          val resultUnevaluated = indexed
            .filterNot { case (index, _) => evaluated.contains(index) }
            .map(_._2)
            .map(pUnevaluated)
          val merge      = processing.nested(keyword)
          val allIndices = Seq.range(0, vs.size)
          mergeAll(merge, Seq(result) ++ resultUnevaluated, value)
            .add(processor.WithPointer(EvaluatedIndices(allIndices), value.pointer))
        case _ => result
      }
  }

  private def checkUnevaluated[R](processing: Processing[R], keyword: UnevaluatedPropertiesKeyword): ProcessFun[R] = {
    value =>
      val p      = all(processing, keyword.pushed)
      val result = p(value)
      value.value match {
        case ObjectValue(vs) =>
          val evaluated = result.annotations
            .filter(_.pointer == value.pointer)
            .flatMap {
              case WithPointer(EvaluatedProperties(properties), _) => properties
              case _                                               => Seq()
            }
            .toSet
          val pUnevaluated = all(processing, keyword.unevaluated)
          val resultUnevaluated = vs
            .filterNot { case (prop, _) => evaluated.contains(prop) }
            .map { case (prop, v) =>
              pUnevaluated(InnerValue(v, value.pointer / prop))
            }
            .toSeq
          val merge         = processing.nested(keyword)
          val allProperties = vs.keySet
          mergeAll(merge, Seq(result) ++ resultUnevaluated, value)
            .add(processor.WithPointer(EvaluatedProperties(allProperties), value.pointer))
        case _ => result
      }
  }
}
