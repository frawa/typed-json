package frawa.typedjson.eval

import frawa.typedjson.keywords.*
import frawa.typedjson.parser.Value
import frawa.typedjson.util.UriUtil
import frawa.typedjson.validation.CannotResolve

import java.net.URI
import frawa.typedjson.output.OutputOps

case class CacheState(
    rootResolver: SchemaResolver,
    rootVocabulary: Vocabulary,
    countBind: Int,
    cache: Map[String, CacheState.Cached],
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
    Stats(binds = state.countBind, hits = state.hits.map(_._2).sum, cached = state.cache.keySet.size)

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
    CacheState(resolver, vocabulary, 0, Map.empty, Map.empty)

  def resolve[O: OutputOps](ref: String, base: URI, scope: DynamicScope)(using
      eval: Eval[R, O]
  ): Eval.Fun[R[O]] =
    doResolve(ref, base, scope) { (ref, resolver) =>
      resolver.resolveRef(ref)
    }

  def resolveDynamic[O: OutputOps](ref: String, base: URI, scope: DynamicScope)(using
      eval: Eval[R, O]
  ): Eval.Fun[R[O]] =
    doResolve(ref, base, scope) { (ref, resolver) => resolver.resolveDynamicRef(ref, scope) }

  def doResolve[O: OutputOps](ref: String, base: URI, scope: DynamicScope)(
      resolve: (String, SchemaResolver) => Option[SchemaResolution]
  )(using
      eval: Eval[R, O]
  ): Eval.Fun[R[O]] = value =>
    state =>
      val ops = summon[OutputOps[O]]
      val uri = UriUtil.absolute(ref, base).toString
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

      lazy val newlyCached = resolve(ref, resolver)
        .map { r =>
          val v = SchemaValue.vocabulary(r, vocabulary)
          (r, v)
        }
        .map { rv =>
          val state1 = state.copy(cache = state.cache + (uri -> rv))
          (rv, state1)
        }
      val compiled = alreadyCached
        .orElse(newlyCached)
        .map { case ((resolution, vocabulary), state) =>
          vocabulary
            .flatMap(vocabulary => Keywords.parseKeywords(vocabulary, resolution, scope))
            .fold(
              { problems =>
                val ref = resolution.resolver.base.toString
                (ops.invalid(CannotResolve(ref, Some(problems)), value.pointer), state)
              },
              { ks =>
                eval.compile(ks)(value)(state)
              }
            )
        }
      compiled.getOrElse {
        // TODO
        // (ops.invalid(MissingReference(ref), value.pointer,state)
        (ops.invalid(CannotResolve(ref, None), value.pointer), state)
      }
