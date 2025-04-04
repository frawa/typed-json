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

import frawa.typedjson.eval.CacheState.R
import frawa.typedjson.eval.CacheState.given
import frawa.typedjson.keywords._
import frawa.typedjson.meta.MetaSchemas
import frawa.typedjson.output.SimpleOutput
import frawa.typedjson.output.SimpleOutput.given
import frawa.typedjson.parser.Value
import frawa.typedjson.parser.Value._
import frawa.typedjson.pointer.Pointer
import frawa.typedjson.testutil.TestSchemas._
import frawa.typedjson.testutil.TestUtil.{_, given}
import frawa.typedjson.util.WithPointer
import frawa.typedjson.validation._
import munit.FunSuite

class EvalResolveTest extends FunSuite:

  import Util.*

  private val evalBasic       = Eval[R, SimpleOutput]
  given Eval[R, SimpleOutput] = evalBasic

  test("missing $id/$ref/$def") {
    withCompiledSchema(missingIdRefDefsSchema) { fun =>
      assertEquals(
        doApply(fun, parseJsonValue("[1313]")),
        SimpleOutput(
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
        SimpleOutput(true, Seq(), annotations = Seq(EvaluatedIndices(Set(0))))
      )
      assertEquals(
        doApply(fun, parseJsonValue("""["hello"]""")),
        SimpleOutput(
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
          SimpleOutput(true, Seq(), annotations = Seq(EvaluatedIndices(Set(0)))),
          SimpleOutput(
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
        SimpleOutput(true, annotations = Seq(EvaluatedProperties(Set("foo"))))
      )
      assertEquals(
        doApply(fun, parseJsonValue("""{ "foo": true }""")),
        SimpleOutput(false, Seq(WithPointer(TypeMismatch("number"), Pointer.empty / "foo")))
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
              Set(
                "https://example.net/root.json#object",
                "https://example.net/root.json#/$defs/numberType"
              )
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
          SimpleOutput(true, annotations = Seq(EvaluatedProperties(Set("foo")))),
          SimpleOutput(true, annotations = Seq(EvaluatedProperties(Set("foo")))),
          SimpleOutput(
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
          { state =>
            assertEquals(
              state.cache.keySet,
              Set(
                "https://json-schema.org/draft/2020-12/meta/validation",
                "https://json-schema.org/draft/2020-12/meta/core",
                "https://json-schema.org/draft/2020-12/meta/validation#/$defs/simpleTypes"
              )
            )
            assertEquals(
              state.hits,
              Map(
                "https://json-schema.org/draft/2020-12/meta/validation"                    -> 5,
                "https://json-schema.org/draft/2020-12/meta/core"                          -> 5,
                "https://json-schema.org/draft/2020-12/meta/validation#/$defs/simpleTypes" -> 3
              )
            )
          }
        ),
        Seq(
          SimpleOutput(true, annotations = Seq(EvaluatedProperties(Set("$defs")))),
          SimpleOutput(true, annotations = Seq(EvaluatedProperties(Set("$defs")))),
          SimpleOutput(
            false,
            Seq(
              WithPointer(
                AdditionalPropertyInvalid("foo"),
                pointer = Pointer.parse("/$defs/foo").get
              ),
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
                pointer = Pointer.parse("/$defs/foo/type").get
              ),
              WithPointer(
                TypeMismatch(
                  expected = "array"
                ),
                pointer = Pointer.parse("/$defs/foo/type").get
              )
            )
          )
        )
      )
    }
  }
