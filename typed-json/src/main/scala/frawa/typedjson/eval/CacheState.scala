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

import frawa.typedjson.keywords._
import frawa.typedjson.output.OutputOps
import frawa.typedjson.pointer.Pointer
import frawa.typedjson.util.UriUtil
import frawa.typedjson.validation.CannotResolve

import java.net.URI
import scala.collection.immutable.Seq

case class CacheState(
    rootResolver: SchemaResolver,
    rootVocabulary: Vocabulary,
    countBind: Int,
    cache: Map[String, CacheState.Cached],
    stack: Seq[(String, Pointer)],
    hits: Map[String, Int]
) {
  def stats: CacheState.Stats =
    CacheState.stats(this)
}

object CacheState:
  type R = [A] =>> CacheState => (A, CacheState)

  type Cached = (SchemaResolution, Either[SchemaProblems, Vocabulary])

  case class Stats(binds: Int, hits: Int, cached: Int)

  def stats(state: CacheState): Stats =
    Stats(
      binds = state.countBind,
      hits = state.hits.map(_._2).sum,
      cached = state.cache.keySet.size
    )

  def unit[A](a: A): R[A] = s => (a, s)

  def bind[A, B](a: R[A])(f: A => R[B]): R[B] = s =>
    val (a2, s2) = a(s)
    f(a2)(s2.copy(countBind = s2.countBind + 1))

  given [O: OutputOps]: TheResultMonad[R, O] with
    def unit[A](a: A): R[A] =
      CacheState.unit(a)

    def bind[A, B](a: R[A])(f: A => R[B]): R[B] =
      CacheState.bind(a)(f)

    def resolve(ref: String, base: URI, scope: DynamicScope)(using
        eval: Eval[R, O]
    ): Eval.Fun[R[O]] =
      CacheState.resolve(ref, base, scope)

    def resolveDynamic(ref: String, base: URI, scope: DynamicScope)(using
        eval: Eval[R, O]
    ): Eval.Fun[R[O]] =
      CacheState.resolveDynamic(ref, base, scope)

  def empty(resolver: SchemaResolver, vocabulary: Vocabulary): CacheState =
    CacheState(resolver, vocabulary, 0, Map.empty, Seq(), Map.empty)

  def resolve[O: OutputOps](ref: String, base: URI, scope: DynamicScope)(using
      eval: Eval[R, O]
  ): Eval.Fun[R[O]] =
    doResolve(ref, base, scope, cacheIt = true) { (ref, resolver) =>
      resolver.resolveRef(ref)
    }

  def resolveDynamic[O: OutputOps](ref: String, base: URI, scope: DynamicScope)(using
      eval: Eval[R, O]
  ): Eval.Fun[R[O]] =
    doResolve(ref, base, scope, cacheIt = false) { (ref, resolver) =>
      resolver.resolveDynamicRef(ref, scope)
    }

  def doResolve[O: OutputOps](ref: String, base: URI, scope: DynamicScope, cacheIt: Boolean)(
      resolve: (String, SchemaResolver) => Option[SchemaResolution]
  )(using
      eval: Eval[R, O]
  ): Eval.Fun[R[O]] = value =>
    state =>
      val ops  = summon[OutputOps[O]]
      val uri0 = UriUtil.absolute(ref, base)
      val uri  = uri0.toString
      val alreadyCached = state.cache
        .get(uri)
        .map { cached =>
          val state1 = state.copy(hits = state.hits.updatedWith(uri)(_.map(_ + 1).orElse(Some(1))))
          (cached, state1)
        }
      lazy val (resolver, vocabulary) =
        state.cache
          .get(base.toString)
          .map(rv => (rv._1.resolver, rv._2.getOrElse(state.rootVocabulary)))
          .getOrElse((state.rootResolver, state.rootVocabulary))

      lazy val newlyCached = resolve(uri, resolver)
        .map { r =>
          val v = SchemaValue.vocabulary(r, vocabulary)
          (r, v)
        }
        .map { rv =>
          val key  = uri
          val key2 = rv._1.resolver.base.toString
          lazy val newCache =
            if key == key2 then {
              state.cache + (key -> rv)
            } else {
              // alias, aka resolved files with different $id
              state.cache + (key -> rv) + (key2 -> rv)
            }
          val state1 = if cacheIt then state.copy(cache = newCache) else state
          (rv, state1)
        }
      val compiled = alreadyCached
        .orElse(newlyCached)
        .map { case ((resolution, vocabulary), state) =>
          vocabulary
            .flatMap { vocabulary =>
              val scope1 = scope.resolved(resolution.resolver.base)
              Keywords.parseKeywords(vocabulary, resolution, scope1).map { ks =>
                (ks, scope1.currentLocation.kl)
              }
            }
            .fold(
              { problems =>
                val ref = resolution.resolver.base.toString
                (ops.invalid(CannotResolve(ref, Some(problems)), value.pointer), state)
              },
              { (ks, kl) =>
                val push = (uri, value.pointer)
                if state.stack.contains(push) then
                  // stop recursion
                  (ops.valid(value.pointer), state)
                else
                  val state1      = state.copy(stack = push +: state.stack)
                  val (o, state2) = eval.compile(ks, kl)(value)(state1)
                  val state3      = state2.copy(stack = state.stack)
                  (o, state3)
              }
            )
        }
      compiled.getOrElse {
        // TODO
        // (ops.invalid(MissingReference(ref), value.pointer,state)
        (ops.invalid(CannotResolve(ref, None), value.pointer), state)
      }
