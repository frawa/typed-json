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

package frawa.typedjson.keywords

import frawa.typedjson
import frawa.typedjson.parser.{ArrayValue, ObjectValue, StringValue}
import frawa.typedjson.keywords
import frawa.typedjson.pointer.Pointer

case class Evaluator[R] private[keywords] (private val eval: Evaluator.EvalFun[R]) {
  def apply(value: InnerValue): Result[R]              = eval(value)
  def andThen(f: Result[R] => Result[R]): Evaluator[R] = this.copy(eval = eval.andThen(f))
}

object Evaluator {
  import Keywords.KeywordWithLocation

  type EvalFun[R]    = InnerValue => Result[R]
  type CombineFun[R] = Seq[Result[R]] => EvalFun[R]

  def apply[R](keywords: Keywords, processing: Processing[R]): Evaluator[R] = {
    Evaluator(all(processing, keywords).andThen(_.addIgnoredKeywords(keywords.ignored, Pointer.empty)))
  }

  private def all[R](processing: Processing[R], keywords: Keywords): EvalFun[R] = { value =>
    seq(keywords.keywords.map(one(processing, _)))
      .andThen(_.addIgnoredKeywords(keywords.ignored, value.pointer))(value)
  }

  private def noop[R]: EvalFun[R] = _ => Result.valid[R]
  private def simple[R](processing: Processing[R], keyword: AssertionKeyword): EvalFun[R] =
    processing.simple(keyword)
  private def seq[R](ps: Seq[EvalFun[R]]): EvalFun[R] = value => Result.combine(ps.map(_(value)))
  private def option[R](p: Option[EvalFun[R]]): EvalFun[R] = {
    p.getOrElse(noop)
  }

  private def applyToArray[R](pPrefix: Seq[EvalFun[R]], pItems: Option[EvalFun[R]])(
      combine: CombineFun[R]
  ): EvalFun[R] = { value: InnerValue =>
    value.value match {
      case ArrayValue(vs) =>
        val indexed = vs.zipWithIndex
          .map { case (v, index) =>
            InnerValue(v, value.pointer / index)
          }
        val resultPrefix = pPrefix.zip(indexed).map { case (p, v) => p(v) }
        val result       = pItems.map(pItems => indexed.drop(pPrefix.size).map(pItems)).getOrElse(Seq())
        val indices      = Seq.range(0, resultPrefix.size + result.size)
        combineAll(combine, resultPrefix ++ result, value).add(WithPointer(EvaluatedIndices(indices)))
      case _ => Result.valid
    }
  }

  // TODO return a EvalFun[R]?
  private def combineAll[R](combine: CombineFun[R], cs: Seq[Result[R]], value: InnerValue): Result[R] = {
    combine(cs)(value).add(cs)
  }

  private def applyToObject[R](
      ps: => PartialFunction[String, () => EvalFun[R]]
  )(combine: CombineFun[R]): EvalFun[R] = { value =>
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
        val annotation    = keywords.WithPointer(evaluatedKeys, value.pointer)
        combineAll(combine, result, value).add(annotation)
      case _ => Result.valid
    }
  }

  private def applyToValue[R](ps: Seq[EvalFun[R]])(combine: CombineFun[R]): EvalFun[R] = { value =>
    val result = ps.map(_(value))
    combineAll(combine, result, value)
  }

  private def applyCondition[R](pIf: EvalFun[R], pThen: EvalFun[R], pElse: EvalFun[R])(
      combine: CombineFun[R]
  ): EvalFun[R] = { value =>
    val ifResult = pIf(value)
    val result   = if (ifResult.valid) Seq(ifResult, pThen(value)) else Seq(pElse(value))
    combineAll(combine, result, value)
  }

  private def one[R](processing: Processing[R], keyword: KeywordWithLocation): EvalFun[R] = {
    keyword.value match {
      case c: AssertionKeyword  => value => simple(processing, c)(value)
      case c: ApplicatorKeyword => value => nesting(processing, c)(value)
    }
  }

  private def nesting[R](processing: Processing[R], keyword: ApplicatorKeyword): EvalFun[R] =
    keyword match {
      case c: ArrayItemsKeyword            => evalArrayItems(processing, c)
      case c: ObjectPropertiesKeyword      => evalObjectProperties(processing, c)
      case c: NotKeyword                   => evalApplicator(processing, Seq(c.keywords))(processing.nested(keyword))
      case c: AllOfKeyword                 => evalApplicator(processing, c.keywords)(processing.nested(keyword))
      case c: AnyOfKeyword                 => evalApplicator(processing, c.keywords)(processing.nested(keyword))
      case c: OneOfKeyword                 => evalApplicator(processing, c.keywords)(processing.nested(keyword))
      case c: UnionTypeKeyword             => evalUnionType(processing, c)(processing.nested(keyword))
      case c: IfThenElseKeyword            => evalIfThenElse(processing, c)
      case c: PropertyNamesKeyword         => evalPropertyNames(processing, c)
      case c: LazyParseKeywords            => evalLazyParseKeywords(processing, c)
      case c: DependentSchemasKeyword      => evalDependentSchemas(processing, c)
      case c: ContainsKeyword              => evalContains(processing, c)
      case c: UnevaluatedItemsKeyword      => evalUnevaluated(processing, c)
      case c: UnevaluatedPropertiesKeyword => evalUnevaluated(processing, c)
    }

  private def evalArrayItems[R](processing: Processing[R], keyword: ArrayItemsKeyword): EvalFun[R] = {
    val pPrefix = keyword.prefixItems.map(all(processing, _))
    val pItems  = keyword.items.map(all(processing, _))
    val combine = processing.nested(keyword)
    applyToArray(pPrefix, pItems)(combine)
  }

  private def evalObjectProperties[R](processing: Processing[R], keyword: ObjectPropertiesKeyword): EvalFun[R] = {
    val psProperties = keyword.properties.map { case (key, keywords) =>
      val partial: PartialFunction[String, () => EvalFun[R]] = {
        case k if k == key =>
          () => all(processing, keywords)
      }
      partial
    }.toSeq

    val psPatterns = keyword.patternProperties.map { case (regex, keywords) =>
      val r = regex.r
      val partial: PartialFunction[String, () => EvalFun[R]] = {
        case k if r.findFirstIn(k).isDefined => () => all(processing, keywords)
      }
      partial
    }.toSeq

    val psBoth = psProperties ++ psPatterns

    val psAll: PartialFunction[String, () => EvalFun[R]] = {
      case k if psBoth.exists(_.isDefinedAt(k)) =>
        () => seq(psBoth.map(_.lift).flatMap { p => p(k).map(_()) })
    }

    val psAdditional = keyword.additionalProperties.map { keywords =>
      val partial: PartialFunction[String, () => EvalFun[R]] = { _ => () => all(processing, keywords) }
      partial
    }

    val ps      = psAdditional.map(psAll.orElse(_)).getOrElse(psAll)
    val combine = processing.nested(keyword)
    value => {
      applyToObject(ps)(combine)(value)
    }
  }

  private def evalApplicator[R](processing: Processing[R], keywords: Seq[Keywords])(
      combine: CombineFun[R]
  ): EvalFun[R] = {
    val p = keywords.map(all(processing, _))
    applyToValue(p)(combine)
  }

  private def evalUnionType[R](processing: Processing[R], keyword: UnionTypeKeyword)(
      combine: CombineFun[R]
  ): EvalFun[R] = {
    val p = keyword.keywords.map(one(processing, _))
    applyToValue(p)(combine)
  }

  private def evalIfThenElse[R](processing: Processing[R], keyword: IfThenElseKeyword): EvalFun[R] = {
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

  private def evalPropertyNames[R](processing: Processing[R], keyword: PropertyNamesKeyword): EvalFun[R] = { value =>
    value.value match {
      case ObjectValue(vs) =>
        val p       = all(processing, keyword.keywords)
        val combine = processing.nested(keyword)
        val names   = vs.keySet
        val result = names.map { name =>
          (name, p(InnerValue(StringValue(name), value.pointer / name)))
        }.toSeq
        val validNames = result.filter(_._2.valid).map(_._1).toSet
        val annotation = keywords.WithPointer(EvaluatedProperties(validNames), value.pointer)
        combineAll(combine, result.map(_._2), value).add(annotation)
      case _ => Result.valid
    }
  }

  private def evalLazyParseKeywords[R](processing: Processing[R], keyword: LazyParseKeywords): EvalFun[R] = {
    keyword.parse() match {
      case Right(keywords) => all(processing, keywords)
      case Left(problems)  => _ => Result.invalid.add(problems)
    }
  }

  private def evalDependentSchemas[R](processing: Processing[R], keyword: DependentSchemasKeyword): EvalFun[R] = {
    value =>
      value.value match {
        case ObjectValue(v) =>
          val ps      = v.keySet.flatMap(keyword.keywords.get).map(all(processing, _)).toSeq
          val combine = processing.nested(keyword)
          val result  = ps.map(_(value))
          combineAll(combine, result, value)
        case _ => Result.valid
      }
  }

  private def evalContains[R](processing: Processing[R], keyword: ContainsKeyword): EvalFun[R] = { value =>
    value.value match {
      case ArrayValue(vs) =>
        keyword.schema
          .map { schema =>
            val p       = all(processing, schema)
            val combine = processing.nested(keyword)
            val indexed = vs.zipWithIndex
              .map { case (v, index) =>
                InnerValue(v, value.pointer / index)
              }
            val result       = indexed.map(p(_))
            val validIndices = result.zipWithIndex.filter(_._1.valid).map(_._2)
            combineAll(combine, result, value).add(
              typedjson.keywords.WithPointer(EvaluatedIndices(validIndices), value.pointer)
            )
          }
          .getOrElse(Result.valid)
      case _ => Result.valid
    }
  }

  private def evalUnevaluated[R](processing: Processing[R], keyword: UnevaluatedItemsKeyword): EvalFun[R] = { value =>
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
        val combine    = processing.nested(keyword)
        val allIndices = Seq.range(0, vs.size)
        combineAll(combine, Seq(result) ++ resultUnevaluated, value)
          .add(keywords.WithPointer(EvaluatedIndices(allIndices), value.pointer))
      case _ => result
    }
  }

  private def evalUnevaluated[R](processing: Processing[R], keyword: UnevaluatedPropertiesKeyword): EvalFun[R] = {
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
          val combine       = processing.nested(keyword)
          val allProperties = vs.keySet
          combineAll(combine, Seq(result) ++ resultUnevaluated, value)
            .add(keywords.WithPointer(EvaluatedProperties(allProperties), value.pointer))
        case _ => result
      }
  }
}
