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

  given [O: OutputOps]: TheResultMonad[MyR[O], O] with
    def unit[A](a: A): MyR[O][A] = s => (a, s)

    def bind[A, B](a: MyR[O][A])(f: A => MyR[O][B]): MyR[O][B] = s =>
      val (a2, s2) = a(s)
      f(a2)(s2.copy(count = s2.count + 1))

    def resolve(ref: String, base: URI, scope: DynamicScope)(using
        eval: Eval[MyR[O], O]
    ): MyR[O][Eval.Fun[O]] =
      MyState.resolve(ref, base, scope) // .flatMap(ff => unit((value: WithPointer[Value]) => ff(value)))

    def resolveDynamic(ref: String, base: URI, scope: DynamicScope)(using
        eval: Eval[MyR[O], O]
    ): MyR[O][Eval.Fun[O]] =
      MyState.resolveDynamic(ref, base, scope) // .flatMap(ff => unit((value: WithPointer[Value]) => ff(value)))

  def myZero[O: OutputOps](resolver: SchemaResolver, vocabulary: Vocabulary): MyState =
    MyState(resolver, vocabulary, 0, Map.empty, Map.empty)

  def resolve[O: OutputOps](ref: String, base: URI, scope: DynamicScope)(using
      eval: Eval[MyR[O], O]
  ): MyR[O][Eval.Fun[O]] =
    doResolve(ref, base, scope) { (ref, resolver) =>
      resolver.resolveRef(ref)
    }

  def resolveDynamic[O: OutputOps](ref: String, base: URI, scope: DynamicScope)(using
      eval: Eval[MyR[O], O]
  ): MyR[O][Eval.Fun[O]] =
    doResolve(ref, base, scope) { (ref, resolver) => resolver.resolveDynamicRef(ref, scope) }

  def doResolve[O: OutputOps](ref: String, base: URI, scope: DynamicScope)(
      resolve: (String, SchemaResolver) => Option[SchemaResolution]
  )(using
      eval: Eval[MyR[O], O]
  ): MyR[O][Eval.Fun[O]] =
    val ops = summon[OutputOps[O]]
    (state: MyState) =>
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
        .map { case ((resolution, vocabulary), state0) =>
          vocabulary
            .flatMap(vocabulary => Keywords.parseKeywords(vocabulary, resolution, scope))
            .fold(
              { problems =>
                val ref = resolution.resolver.base.toString
                val f: Eval.Fun[O] = { (value: WithPointer[Value]) =>
                  ops.invalid(CannotResolve(ref, Some(problems)), value.pointer)
                }
                (f, state0)
              },
              { ks =>
                val f: MyR[O][Eval.Fun[O]] = { state =>
                  val (compiled, state1) = eval.compile(ks)(state)
                  (value => compiled(value), state1)
                }
                f(state0)
              }
            )
        }
      compiled.getOrElse {
        // TODO
        // val f = (value: WithPointer[Value]) => ops.invalid(MissingReference(ref), value.pointer)
        val f = (value: WithPointer[Value]) => ops.invalid(CannotResolve(ref, None), value.pointer)
        (f, state)
      }
