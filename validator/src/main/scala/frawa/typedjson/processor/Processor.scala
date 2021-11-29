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

case class Processor[R] private[processor] (private val process: Processor.ProcessFun[R]) {
  def apply(value: InnerValue): Result[R]              = process(value)
  def andThen(f: Result[R] => Result[R]): Processor[R] = this.copy(process = process.andThen(f))
}

object Processor {
  import Keywords.KeywordWithLocation

  type ProcessFun[R] = InnerValue => Result[R]
  type MergeFun[R]   = Seq[Result[R]] => ProcessFun[R]

  def apply[R](schema: SchemaValue, lazyResolver: Option[LoadedSchemasResolver.LazyResolver] = None)(
      eval: Eval[R]
  ): Either[SchemaProblems, Processor[R]] = {
    implicit val resolver: LoadedSchemasResolver = LoadedSchemasResolver(schema, lazyResolver)

    val scope = DynamicScope.empty.push(resolver.base)
    for {
      vocabulary <- SchemaValue.vocabulary(schema, Vocabulary.coreVocabulary)
      keywords   <- Keywords.parseKeywords(vocabulary, schema, scope)
      processor = Processor(all(eval, keywords).andThen(_.addIgnoredKeywords(keywords.ignored, Pointer.empty)))
    } yield processor
  }

  private def all[R](eval: Eval[R], keywords: Keywords): ProcessFun[R] = { value =>
    seq(keywords.keywords.map(one(eval, _)))
      .andThen(_.addIgnoredKeywords(keywords.ignored, value.pointer))(value)
  }

  private def noop[R]: ProcessFun[R]                                          = _ => Result.valid[R]
  private def simple[R](eval: Eval[R], keyword: SimpleKeyword): ProcessFun[R] = eval.simple(keyword)
  private def seq[R](ps: Seq[ProcessFun[R]]): ProcessFun[R]                   = value => Result.merge(ps.map(_(value)))
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

  private def one[R](eval: Eval[R], keyword: KeywordWithLocation): ProcessFun[R] = {
    keyword.value match {
      case c: SimpleKeyword  => value => simple(eval, c)(value)
      case c: NestingKeyword => value => nesting(eval, c)(value)
    }
  }

  private def nesting[R](eval: Eval[R], keyword: NestingKeyword): ProcessFun[R] =
    keyword match {
      case c: ArrayItemsKeyword            => checkArrayItems(eval, c)
      case c: ObjectPropertiesKeyword      => checkObjectProperties(eval, c)
      case c: NotKeyword                   => checkApplicator(eval, Seq(c.keywords))(eval.nested(keyword))
      case c: AllOfKeyword                 => checkApplicator(eval, c.keywords)(eval.nested(keyword))
      case c: AnyOfKeyword                 => checkApplicator(eval, c.keywords)(eval.nested(keyword))
      case c: OneOfKeyword                 => checkApplicator(eval, c.keywords)(eval.nested(keyword))
      case c: UnionTypeKeyword             => checkUnionType(eval, c)(eval.nested(keyword))
      case c: IfThenElseKeyword            => checkIfThenElse(eval, c)
      case c: PropertyNamesKeyword         => checkPropertyNames(eval, c)
      case c: LazyParseKeywords            => checkLazyParseKeywords(eval, c)
      case c: DependentSchemasKeyword      => checkDependentSchemas(eval, c)
      case c: ContainsKeyword              => checkContains(eval, c)
      case c: UnevaluatedItemsKeyword      => checkUnevaluated(eval, c)
      case c: UnevaluatedPropertiesKeyword => checkUnevaluated(eval, c)
    }

  private def checkArrayItems[R](eval: Eval[R], keyword: ArrayItemsKeyword): ProcessFun[R] = {
    val pPrefix = keyword.prefixItems.map(all(eval, _))
    val pItems  = keyword.items.map(all(eval, _))
    val merge   = eval.nested(keyword)
    applyToArray(pPrefix, pItems)(merge)
  }

  private def checkObjectProperties[R](eval: Eval[R], keyword: ObjectPropertiesKeyword): ProcessFun[R] = {
    val psProperties = keyword.properties.map { case (key, keywords) =>
      val partial: PartialFunction[String, () => ProcessFun[R]] = {
        case k if k == key =>
          () => all(eval, keywords)
      }
      partial
    }.toSeq

    val psPatterns = keyword.patternProperties.map { case (regex, keywords) =>
      val r = regex.r
      val partial: PartialFunction[String, () => ProcessFun[R]] = {
        case k if r.findFirstIn(k).isDefined => () => all(eval, keywords)
      }
      partial
    }.toSeq

    val psBoth = psProperties ++ psPatterns

    val psAll: PartialFunction[String, () => ProcessFun[R]] = {
      case k if psBoth.exists(_.isDefinedAt(k)) =>
        () => seq(psBoth.map(_.lift).flatMap { p => p(k).map(_()) })
    }

    val psAdditional = keyword.additionalProperties.map { keywords =>
      val partial: PartialFunction[String, () => ProcessFun[R]] = { _ => () => all(eval, keywords) }
      partial
    }

    val ps    = psAdditional.map(psAll.orElse(_)).getOrElse(psAll)
    val merge = eval.nested(keyword)
    value => {
      applyToObject(ps)(merge)(value)
    }
  }

  private def checkApplicator[R](eval: Eval[R], keywords: Seq[Keywords])(merge: MergeFun[R]): ProcessFun[R] = {
    val p = keywords.map(all(eval, _))
    applyToValue(p)(merge)
  }

  private def checkUnionType[R](eval: Eval[R], keyword: UnionTypeKeyword)(merge: MergeFun[R]): ProcessFun[R] = {
    val p = keyword.keywords.map(one(eval, _))
    applyToValue(p)(merge)
  }

  private def checkIfThenElse[R](eval: Eval[R], keyword: IfThenElseKeyword): ProcessFun[R] = {
    keyword.ifKeywords
      .map(ifChecks =>
        applyCondition(
          all(eval, ifChecks),
          option(keyword.thenKeywords.map(all(eval, _))),
          option(keyword.elseKeywords.map(all(eval, _)))
        )(eval.nested(keyword))
      )
      .getOrElse(noop)
  }

  private def checkPropertyNames[R](eval: Eval[R], keyword: PropertyNamesKeyword): ProcessFun[R] = { value =>
    value.value match {
      case ObjectValue(vs) =>
        val p     = all(eval, keyword.keywords)
        val merge = eval.nested(keyword)
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

  private def checkLazyParseKeywords[R](eval: Eval[R], keyword: LazyParseKeywords): ProcessFun[R] = {
    keyword.parse() match {
      case Right(keywords) => all(eval, keywords)
      case Left(problems)  => _ => Result.invalid.add(problems)
    }
  }

  private def checkDependentSchemas[R](eval: Eval[R], keyword: DependentSchemasKeyword): ProcessFun[R] = { value =>
    value.value match {
      case ObjectValue(v) =>
        val ps     = v.keySet.flatMap(keyword.keywords.get).map(all(eval, _)).toSeq
        val merge  = eval.nested(keyword)
        val result = ps.map(_(value))
        mergeAll(merge, result, value)
      case _ => Result.valid
    }
  }

  private def checkContains[R](eval: Eval[R], keyword: ContainsKeyword): ProcessFun[R] = { value =>
    value.value match {
      case ArrayValue(vs) =>
        keyword.schema
          .map { schema =>
            val p     = all(eval, schema)
            val merge = eval.nested(keyword)
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

  private def checkUnevaluated[R](eval: Eval[R], keyword: UnevaluatedItemsKeyword): ProcessFun[R] = { value =>
    val p      = all(eval, keyword.pushed)
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
        val pUnevaluated = all(eval, keyword.unevaluated)
        val indexed = vs.zipWithIndex
          .map { case (v, index) =>
            (index, InnerValue(v, value.pointer / index))
          }
        val resultUnevaluated = indexed
          .filterNot { case (index, _) => evaluated.contains(index) }
          .map(_._2)
          .map(pUnevaluated)
        val merge      = eval.nested(keyword)
        val allIndices = Seq.range(0, vs.size)
        mergeAll(merge, Seq(result) ++ resultUnevaluated, value)
          .add(processor.WithPointer(EvaluatedIndices(allIndices), value.pointer))
      case _ => result
    }
  }

  private def checkUnevaluated[R](eval: Eval[R], keyword: UnevaluatedPropertiesKeyword): ProcessFun[R] = { value =>
    val p      = all(eval, keyword.pushed)
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
        val pUnevaluated = all(eval, keyword.unevaluated)
        val resultUnevaluated = vs
          .filterNot { case (prop, _) => evaluated.contains(prop) }
          .map { case (prop, v) =>
            pUnevaluated(InnerValue(v, value.pointer / prop))
          }
          .toSeq
        val merge         = eval.nested(keyword)
        val allProperties = vs.keySet
        mergeAll(merge, Seq(result) ++ resultUnevaluated, value)
          .add(processor.WithPointer(EvaluatedProperties(allProperties), value.pointer))
      case _ => result
    }
  }
}
