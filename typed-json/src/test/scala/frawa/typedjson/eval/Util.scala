package frawa.typedjson.eval

import frawa.typedjson.eval.CacheState.R
import frawa.typedjson.keywords.*
import frawa.typedjson.parser.Value
import frawa.typedjson.testutil.TestUtil.{*, given}
import frawa.typedjson.util.UriUtil
import frawa.typedjson.validation.CannotResolve

import java.net.URI
import frawa.typedjson.eval.CacheState

object Util:
  val vocabularyForTest: Option[Vocabulary] = dialect(
    Seq(Vocabulary.coreId, Vocabulary.validationId, Vocabulary.applicatorId, Vocabulary.unevaluatedId)
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
      withCompiledSchemaValue(schema, lazyResolver)(f)
    }

  def withCompiledSchemaValue[R[_], O](
      schema: SchemaValue,
      lazyResolver: Option[LoadedSchemasResolver.LazyResolver] = None
  )(using eval: Eval[R, O])(using TheResultMonad[R, O])(f: AssertingFun[R, O]): Unit =
    withKeywords(schema, lazyResolver) { keywords =>
      val compiled         = eval.compile(keywords)
      val fun              = eval.fun(compiled)
      given SchemaResolver = LoadedSchemasResolver(schema, lazyResolver)
      f(fun)
    }

  import frawa.typedjson.eval.CacheState.{*, given}

  def doApply[O: OutputOps](fun: Value => R[O], value: Value)(using resolver: SchemaResolver): O =
    val (o, s) = fun(value)(empty(resolver, vocabularyForTest.get))
    // println(s"counted ${s.count} binds")
    o

  def doApplyBulk[O: OutputOps](fun: Value => R[O], values: Seq[Value], fun2: CacheState => Unit)(using
      resolver: SchemaResolver
  ): Seq[O] =
    val emptyCache = empty(resolver, vocabularyForTest.get)
    val (s, os) = values
      .foldLeft((emptyCache, Seq.empty[O])) { case ((state, os), v) =>
        val (o, state1) = fun(v)(state)
        (state1, os :+ o)
      }
    fun2(s)
    os
