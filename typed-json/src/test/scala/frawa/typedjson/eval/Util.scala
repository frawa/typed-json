package frawa.typedjson.eval

import frawa.typedjson.eval.MyState.MyR
import frawa.typedjson.keywords.*
import frawa.typedjson.parser.Value
import frawa.typedjson.testutil.TestUtil.{*, given}
import frawa.typedjson.util.UriUtil
import frawa.typedjson.validation.CannotResolve

import java.net.URI

object Util:
  val vocabularyForTest: Option[Vocabulary] = dialect(
    Seq(Vocabulary.coreId, Vocabulary.validationId, Vocabulary.applicatorId)
  )

  def withKeywords(schema: SchemaValue, lazyResolver: Option[LoadedSchemasResolver.LazyResolver] = None)(
      f: Keywords => Unit
  ): Unit =
    // TODO avoid lazyResolver
    Keywords(schema, vocabularyForTest, lazyResolver)
      .fold(
        errors => throw new IllegalArgumentException(s"no keywords: $errors"),
        keywords => f(keywords)
      )

  type AssertingFun[R[_], O] = SchemaResolver ?=> (Value => R[O]) => Unit

  def withCompiledSchema[R[_], O](
      schema: String,
      lazyResolver: Option[LoadedSchemasResolver.LazyResolver] = None
  )(using eval: Eval[R, O])(using TheResultMonad[R, O])(f: AssertingFun[R, O]): Unit =
    withSchema(schema) { schema =>
      withKeywords(schema, lazyResolver) { keywords =>
        val compiled         = eval.compile(keywords)
        val fun              = eval.fun(compiled)
        given SchemaResolver = LoadedSchemasResolver(schema, lazyResolver)
        f(fun)
      }
    }
