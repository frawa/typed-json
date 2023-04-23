package frawa.typedjson.eval

import frawa.typedjson.eval.MyState.{Cached, MyR}
import frawa.typedjson.keywords.*
import frawa.typedjson.parser.Value
import frawa.typedjson.util.UriUtil
import frawa.typedjson.validation.CannotResolve

import java.net.URI

// TODO possible?
// case class MyState[O: OutputOps](resolver: SchemaResolver, count: Int, resolved: Map[String, MyR[Eval.Fun[O]]])
case class MyState(
    rootResolver: SchemaResolver,
    rootVocabulary: Vocabulary,
    count: Int,
    cache: Map[String, MyState.Cached],
    hits: Map[String, Int]
)

object MyState:
  type MyR[O] = [A] =>> MyState => (A, MyState)

  type Cached = (SchemaResolution, Either[SchemaProblems, Vocabulary])

  def unit[A](a: A): MyR[A][A] = s => (a, s)

  def bind[A, B](a: MyR[A][A])(f: A => MyR[B][B]): MyR[B][B] = s =>
    val (a2, s2) = a(s)
    f(a2)(s2.copy(count = s2.count + 1))

  given [O: OutputOps]: TheResultMonad[MyR[O], O] with
    def unit[A](a: A): MyR[O][A] =
      MyState.unit(a)

    def bind[A, B](a: MyR[O][A])(f: A => MyR[O][B]): MyR[O][B] =
      MyState.bind(a)(f)

    def resolve(ref: String, base: URI, scope: DynamicScope)(using
        eval: Eval[MyR[O], O]
    ): Eval.Fun[MyR[O][O]] =
      MyState.resolve(ref, base, scope) // .flatMap(ff => unit((value: WithPointer[Value]) => ff(value)))

    def resolveDynamic(ref: String, base: URI, scope: DynamicScope)(using
        eval: Eval[MyR[O], O]
    ): Eval.Fun[MyR[O][O]] =
      MyState.resolveDynamic(ref, base, scope) // .flatMap(ff => unit((value: WithPointer[Value]) => ff(value)))

  def myZero[O: OutputOps](resolver: SchemaResolver, vocabulary: Vocabulary): MyState =
    MyState(resolver, vocabulary, 0, Map.empty, Map.empty)

  def resolve[O: OutputOps](ref: String, base: URI, scope: DynamicScope)(using
      eval: Eval[MyR[O], O]
  ): Eval.Fun[MyR[O][O]] =
    doResolve(ref, base, scope) { (ref, resolver) =>
      resolver.resolveRef(ref)
    }

  def resolveDynamic[O: OutputOps](ref: String, base: URI, scope: DynamicScope)(using
      eval: Eval[MyR[O], O]
  ): Eval.Fun[MyR[O][O]] =
    doResolve(ref, base, scope) { (ref, resolver) => resolver.resolveDynamicRef(ref, scope) }

  def doResolve[O: OutputOps](ref: String, base: URI, scope: DynamicScope)(
      resolve: (String, SchemaResolver) => Option[SchemaResolution]
  )(using
      eval: Eval[MyR[O], O]
  ): Eval.Fun[MyR[O][O]] = value =>
    state =>
      val ops = summon[OutputOps[O]]
      val uri = UriUtil.absolute(ref, base).toString()
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
