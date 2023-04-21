package frawa.typedjson.eval

import frawa.typedjson.eval.MyState.MyR
import frawa.typedjson.keywords.*
import frawa.typedjson.parser.Value
import frawa.typedjson.testutil.TestUtil.*
import frawa.typedjson.util.UriUtil
import frawa.typedjson.validation.CannotResolve

import java.net.URI

object Util:
  val vocabularyForTest = dialect(Seq(Vocabulary.coreId, Vocabulary.validationId, Vocabulary.applicatorId))
  // val vocabularyForTest = dialect(Seq(Vocabulary.coreId, Vocabulary.validationId))

  def withKeywords(schema: SchemaValue, lazyResolver: Option[LoadedSchemasResolver.LazyResolver] = None)(
    f: Keywords => Unit
  ): Unit =
  // TODO avoid lazyResolver
    Keywords(schema, vocabularyForTest, lazyResolver)
      .fold(
        errors => throw new IllegalArgumentException(s"no keywords: $errors"),
        keywords => f(keywords)
      )

  type AssertingFun[R[_], O] = SchemaResolver ?=> R[Value => O] => Unit

  def withCompiledSchema[R[_], O](
                                   schema: String,
                                   lazyResolver: Option[LoadedSchemasResolver.LazyResolver] = None
                                 )(using eval: Eval[R, O])(using TheResultMonad[R, O])(f: AssertingFun[R, O]): Unit =
    withSchema(schema) { schema =>
      withKeywords(schema, lazyResolver) { keywords =>
        val c = eval.compile(keywords)
        val fun = eval.fun(c)

        given SchemaResolver = LoadedSchemasResolver(schema, lazyResolver)

        f(fun)
      }
    }

  given[O: OutputOps]: TheResultMonad[MyR[O], O] with
    def unit[A](a: A): MyR[O][A] = s => (a, s)

    def bind[A, B](a: MyR[O][A])(f: A => MyR[O][B]): MyR[O][B] = s =>
      val (a2, s2) = a(s)
      f(a2)(s2.copy(count = s2.count + 1))

    def resolve(ref: String, base: URI, scope: DynamicScope)(using
                                                             eval: Eval[MyR[O], O]
    ): MyR[O][Eval.Fun[O]] =
      MyState.resolve(ref, base, scope)

    def resolveDynamic(ref: String, base: URI, scope: DynamicScope)(using
                                                                    eval: Eval[MyR[O], O]
    ): MyR[O][Eval.Fun[O]] =
      MyState.resolveDynamic(ref, base, scope)
