package frawa.typedjson.eval

import frawa.typedjson.eval.*
import frawa.typedjson.eval.CacheState.{R, given}
import frawa.typedjson.keywords.*
import frawa.typedjson.meta.MetaSchemas
import frawa.typedjson.parser.Value
import frawa.typedjson.parser.Value.*
import frawa.typedjson.pointer.Pointer
import frawa.typedjson.testutil.TestSchemas.*
import frawa.typedjson.testutil.TestUtil.{*, given}
import frawa.typedjson.validation.*
import munit.FunSuite
import frawa.typedjson.eval.CacheState
import frawa.typedjson.output.BasicOutput
import frawa.typedjson.output.BasicOutput.given
import frawa.typedjson.util.WithPointer

class EvalResolveTest extends FunSuite:

  import Util.*

  private val evalBasic      = Eval[R, BasicOutput]
  given Eval[R, BasicOutput] = evalBasic

  test("missing $id/$ref/$def") {
    withCompiledSchema(missingIdRefDefsSchema) { fun =>
      assertEquals(
        doApply(fun, parseJsonValue("[1313]")),
        BasicOutput(
          false,
          Seq(WithPointer(CannotResolve("#missing", None), Pointer.empty / 0))
        )
      )
    }
  }

  test("$id/$ref/$def") {
    withCompiledSchema(idRefDefsSchema) { fun =>
      assertEquals(
        doApply(fun, parseJsonValue("[1313]")),
        BasicOutput(true, Seq(), annotations = Seq(EvaluatedIndices(Set(0))))
      )
      assertEquals(
        doApply(fun, parseJsonValue("""["hello"]""")),
        BasicOutput(
          false,
          Seq(WithPointer(TypeMismatch("number"), Pointer.empty / 0))
        )
      )
    }
  }

  test("$id/$ref/$def bulk") {
    withCompiledSchema(idRefDefsSchema) { fun =>
      assertEquals(
        doApplyBulk(
          fun,
          Seq(parseJsonValue("[1313]"), parseJsonValue("""["hello"]""")),
          state =>
            assertEquals(state.cache.keySet, Set("https://example.net/root.json#item"))
            assertEquals(state.hits, Map("https://example.net/root.json#item" -> 1))
        ),
        Seq(
          BasicOutput(true, Seq(), annotations = Seq(EvaluatedIndices(Set(0)))),
          BasicOutput(
            false,
            Seq(WithPointer(TypeMismatch("number"), Pointer.empty / 0))
          )
        )
      )
    }
  }

  test("$ref in properties") {
    withCompiledSchema(refInPropertiesSchema) { fun =>
      assertEquals(
        doApply(fun, parseJsonValue("""{ "foo": 13 }""")),
        BasicOutput(true, annotations = Seq(EvaluatedProperties(Set("foo"))))
      )
      assertEquals(
        doApply(fun, parseJsonValue("""{ "foo": true }""")),
        BasicOutput(false, Seq(WithPointer(TypeMismatch("number"), Pointer.empty / "foo")))
      )
    }
  }

  test("$ref at root") {
    withCompiledSchema(refAtRootSchema) { fun =>
      assertEquals(
        doApplyBulk(
          fun,
          Seq(
            parseJsonValue("""{ "foo": 13 }"""),
            parseJsonValue("""{ "foo": [13] }"""),
            parseJsonValue("""{ "foo": true }""")
          ),
          state =>
            assertEquals(
              state.cache.keySet,
              Set("https://example.net/root.json#object", "https://example.net/root.json#/$defs/numberType")
            )
            assertEquals(
              state.hits,
              Map(
                "https://example.net/root.json#/$defs/numberType" -> 3,
                "https://example.net/root.json#object"            -> 2
              )
            )
        ),
        Seq(
          BasicOutput(true, annotations = Seq(EvaluatedProperties(Set("foo")))),
          BasicOutput(true, annotations = Seq(EvaluatedProperties(Set("foo")))),
          BasicOutput(
            false,
            Seq(
              WithPointer(TypeMismatch("number"), Pointer.empty / "foo"),
              WithPointer(TypeMismatch("array"), Pointer.empty / "foo")
            )
          )
        )
      )
    }
  }

  test("$ref to validation spec, with two '$ref's") {
    val lazyResolver = Some(MetaSchemas.lazyResolver)
    withCompiledSchema(refToValidationSpec, lazyResolver) { fun =>
      assertEquals(
        doApplyBulk(
          fun,
          Seq(
            parseJsonValue("""{ "$defs": { "foo": { "type": "boolean" } } }"""),
            parseJsonValue("""{ "$defs": { "foo": { "type": ["boolean"] } } }"""),
            parseJsonValue("""{ "$defs": { "foo": { "type": 13 } } }""")
          ),
          state =>
            assertEquals(
              state.cache.keySet,
              Set(
                "https://json-schema.org/draft/2020-12/meta/core",
                "https://json-schema.org/draft/2020-12/meta/core#meta",
                "https://json-schema.org/draft/2020-12/meta/validation",
                "https://json-schema.org/draft/2020-12/meta/validation#/$defs/simpleTypes"
              )
            )
            assertEquals(
              state.hits,
              Map(
                "https://json-schema.org/draft/2020-12/meta/core"                          -> 5,
                "https://json-schema.org/draft/2020-12/meta/validation"                    -> 5,
                "https://json-schema.org/draft/2020-12/meta/core#meta"                     -> 2,
                "https://json-schema.org/draft/2020-12/meta/validation#/$defs/simpleTypes" -> 3
              )
            )
        ),
        Seq(
          BasicOutput(true, annotations = Seq(EvaluatedProperties(Set("$defs")))),
          BasicOutput(true, annotations = Seq(EvaluatedProperties(Set("$defs")))),
          BasicOutput(
            false,
            Seq(
              WithPointer(
                NotInEnum(
                  values = Seq(
                    "array",
                    "boolean",
                    "integer",
                    "null",
                    "number",
                    "object",
                    "string"
                  ).map(StringValue.apply)
                ),
                pointer = Pointer.parse("/$defs/foo/type")
              ),
              WithPointer(
                TypeMismatch(
                  expected = "array"
                ),
                pointer = Pointer.parse("/$defs/foo/type")
              )
            )
          )
        )
      )
    }
  }
