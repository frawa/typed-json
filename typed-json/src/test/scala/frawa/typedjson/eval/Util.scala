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

import frawa.typedjson.eval.CacheState
import frawa.typedjson.keywords._
import frawa.typedjson.output.OutputOps
import frawa.typedjson.parser.Value
import frawa.typedjson.testutil.TestUtil.{_, given}

import java.net.URI
import scala.collection.immutable.Seq

object Util:
  val vocabularyForTest: Option[Vocabulary] = dialect(
    Seq(
      Vocabulary.coreId,
      Vocabulary.validationId,
      Vocabulary.applicatorId,
      Vocabulary.unevaluatedId
    )
  )

  def withKeywords[T](
      schema: SchemaValue,
      lazyResolver: Option[LoadedSchemasResolver.LazyResolver] = None,
      vocabulary: Option[Vocabulary] = None
  )(
      f: Keywords => T
  ): T =
    val useVocabulary = vocabulary.orElse(vocabularyForTest)
    // TODO avoid lazyResolver
    Keywords(schema, useVocabulary, lazyResolver)
      .fold(
        errors => throw new IllegalArgumentException(s"no keywords: $errors"),
        keywords => f(keywords)
      )

  type AssertingFun[R[_], O, T] = SchemaResolver ?=> (Value => R[O]) => T

  def withCompiledSchema[R[_], O](
      schema: String,
      lazyResolver: Option[LoadedSchemasResolver.LazyResolver] = None
  )(using eval: Eval[R, O])(using TheResultMonad[R, O])(f: AssertingFun[R, O, Unit]): Unit =
    withSchema(schema) { schema =>
      withCompiledSchemaValue(schema, lazyResolver)(f)
    }

  def withCompiledSchemaValue[R[_], O, T](
      schema: SchemaValue,
      lazyResolver: Option[LoadedSchemasResolver.LazyResolver] = None,
      vocabulary: Option[Vocabulary] = None
  )(using eval: Eval[R, O])(using TheResultMonad[R, O])(f: AssertingFun[R, O, T]): T =
    withKeywords(schema, lazyResolver, vocabulary) { keywords =>
      val compiled         = eval.compile(keywords, KeywordLocation.empty)
      val fun              = eval.fun(compiled)
      given SchemaResolver = LoadedSchemasResolver(schema, lazyResolver)
      f(fun)
    }

  import frawa.typedjson.eval.CacheState.*

  def doApplyWithStats[O: OutputOps](fun: Value => R[O], value: Value)(using
      resolver: SchemaResolver
  ): (O, CacheState.Stats) =
    val (o, s) = fun(value)(empty(resolver, vocabularyForTest.get))
    (o, s.stats)

  def doApply[O: OutputOps](fun: Value => R[O], value: Value)(using resolver: SchemaResolver): O =
    val (o, s) = fun(value)(empty(resolver, vocabularyForTest.get))
    // println(s"counted ${s.count} binds")
    o

  def doApplyBulk[O: OutputOps](fun: Value => R[O], values: Seq[Value], fun2: CacheState => Unit)(
      using resolver: SchemaResolver
  ): Seq[O] =
    val emptyCache = empty(resolver, vocabularyForTest.get)
    val (s, os) = values
      .foldLeft((emptyCache, Seq.empty[O])) { case ((state, os), v) =>
        val (o, state1) = fun(v)(state)
        (state1, os :+ o)
      }
    fun2(s)
    os
