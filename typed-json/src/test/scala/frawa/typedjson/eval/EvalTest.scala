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

import frawa.typedjson.eval.*
import frawa.typedjson.eval.MyState.{MyR, given}
import frawa.typedjson.keywords.*
import frawa.typedjson.keywords.SchemaProblems.MissingReference
import frawa.typedjson.meta.MetaSchemas
import frawa.typedjson.parser.Value
import frawa.typedjson.parser.Value.*
import frawa.typedjson.pointer.Pointer
import frawa.typedjson.testutil.TestSchemas.*
import frawa.typedjson.testutil.TestUtil.{*, given}
import frawa.typedjson.util.UriUtil
import frawa.typedjson.util.UriUtil.uri
import frawa.typedjson.validation.*
import munit.FunSuite

import java.net.URI
import scala.reflect.TypeTest

class EvalTest extends FunSuite:

  import Util.{*, given}
  // import FlagOutput.given
  // import BasicOutput.given

  val evalFlag  = Eval[MyR, FlagOutput]
  val evalBasic = Eval[MyR, BasicOutput]

  given Eval[MyR, BasicOutput] = evalBasic

  test("null") {
    given Eval[MyR, FlagOutput] = evalFlag
    withCompiledSchema(nullSchema) { fun =>
      assertEquals(doApply(fun, NullValue), FlagOutput(true))
      assertEquals(doApply(fun, BoolValue(true)), FlagOutput(false))
    }
  }

  test("true") {
    given Eval[MyR, FlagOutput] = evalFlag
    withCompiledSchema(trueSchema) { fun =>
      assertEquals(doApply(fun, BoolValue(true)), FlagOutput(true))
      assertEquals(doApply(fun, NullValue), FlagOutput(true))
    }
  }

  test("null with errors") {
    withCompiledSchema(nullSchema) { fun =>
      assertEquals(doApply(fun, NullValue), BasicOutput(true, Seq()))
      assertEquals(
        doApply(fun, BoolValue(true)),
        BasicOutput(
          false,
          Seq(WithPointer(TypeMismatch("null")))
        )
      )
    }
  }

  test("false") {
    given Eval[MyR, FlagOutput] = evalFlag
    withCompiledSchema(falseSchema) { fun =>
      assertEquals(doApply(fun, BoolValue(true)), FlagOutput(false))
      assertEquals(doApply(fun, NullValue), FlagOutput(false))
      assertEquals(doApply(fun, parseJsonValue("13")), FlagOutput(false))
    }
  }

  test("false with errors") {
    withCompiledSchema(falseSchema) { fun =>
      assertEquals(doApply(fun, parseJsonValue("{}")), BasicOutput(false, Seq(WithPointer(FalseSchemaReason()))))
    }
  }

  test("boolean") {
    withCompiledSchema(boolSchema) { fun =>
      assertEquals(doApply(fun, parseJsonValue("true")), BasicOutput(true, Seq()))
      assertEquals(doApply(fun, parseJsonValue("13")), BasicOutput(false, Seq(WithPointer(TypeMismatch("boolean")))))
    }
  }

  test("true schema") {
    withCompiledSchema(trueSchema) { fun =>
      assertEquals(doApply(fun, parseJsonValue("null")), BasicOutput(true, Seq()))
      assertEquals(doApply(fun, parseJsonValue("13")), BasicOutput(true, Seq()))
      assertEquals(doApply(fun, parseJsonValue("{}")), BasicOutput(true, Seq()))
    }
  }

  test("false schema") {
    withCompiledSchema(falseSchema) { fun =>
      assertEquals(doApply(fun, parseJsonValue("null")), BasicOutput(false, Seq(WithPointer(FalseSchemaReason()))))
      assertEquals(doApply(fun, parseJsonValue("13")), BasicOutput(false, Seq(WithPointer(FalseSchemaReason()))))
      assertEquals(doApply(fun, parseJsonValue("{}")), BasicOutput(false, Seq(WithPointer(FalseSchemaReason()))))
    }
  }

  test("not false") {
    given Eval[MyR, FlagOutput] = evalFlag
    withCompiledSchema(notFalseSchema) { fun =>
      assertEquals(doApply(fun, parseJsonValue("null")), FlagOutput(true))
      assertEquals(doApply(fun, parseJsonValue("13")), FlagOutput(true))
      assertEquals(doApply(fun, parseJsonValue("{}")), FlagOutput(true))
    }
  }

  test("empty schema") {
    withCompiledSchema(emtpySchema) { fun =>
      assertEquals(doApply(fun, parseJsonValue("null")), BasicOutput(true, Seq()))
      assertEquals(doApply(fun, parseJsonValue("13")), BasicOutput(true, Seq()))
      assertEquals(doApply(fun, parseJsonValue("{}")), BasicOutput(true, Seq()))
    }
  }

  test("not empty schema") {
    withCompiledSchema("""{"not": {}}""") { fun =>
      assertEquals(doApply(fun, parseJsonValue("null")), BasicOutput(false, Seq(WithPointer(NotInvalid()))))
      assertEquals(doApply(fun, parseJsonValue("13")), BasicOutput(false, Seq(WithPointer(NotInvalid()))))
      assertEquals(doApply(fun, parseJsonValue("{}")), BasicOutput(false, Seq(WithPointer(NotInvalid()))))
    }
  }

  test("string") {
    withCompiledSchema(stringSchema) { fun =>
      assertEquals(doApply(fun, parseJsonValue(""""hello"""")), BasicOutput(true, Seq()))
      assertEquals(doApply(fun, parseJsonValue("13")), BasicOutput(false, Seq(WithPointer(TypeMismatch("string")))))
    }
  }

  test("number") {
    withCompiledSchema(numberSchema) { fun =>
      assertEquals(doApply(fun, parseJsonValue("13")), BasicOutput(true, Seq()))
      assertEquals(doApply(fun, parseJsonValue("null")), BasicOutput(false, Seq(WithPointer(TypeMismatch("number")))))
    }
  }

  test("array") {
    withCompiledSchema(numberArraySchema) { fun =>
      assertEquals(doApply(fun, parseJsonValue("[13]")), BasicOutput(true, Seq()))
      assertEquals(doApply(fun, parseJsonValue("null")), BasicOutput(false, Seq(WithPointer(TypeMismatch("array")))))
    }
  }

  test("array items") {
    withCompiledSchema(numberArraySchema) { fun =>
      assertEquals(doApply(fun, parseJsonValue("null")), BasicOutput(false, Seq(WithPointer(TypeMismatch("array")))))
      assertEquals(doApply(fun, parseJsonValue("[13]")), BasicOutput(true, Seq()))
      assertEquals(
        doApply(fun, parseJsonValue("[true]")),
        BasicOutput(false, Seq(WithPointer(TypeMismatch("number"), Pointer.empty / 0)))
      )
    }
  }

  test("object") {
    withCompiledSchema(totoObjectSchema) { fun =>
      assertEquals(
        doApply(
          fun,
          parseJsonValue("""{
                           |"toto": 13,
                           |"titi": "hello"
                           |}
                           |""".stripMargin)
        ),
        BasicOutput(true, Seq())
      )
      assertEquals(
        doApply(fun, parseJsonValue("null")),
        BasicOutput(false, Seq(WithPointer(TypeMismatch("object"))))
      )
    }
  }

  test("object with pointer") {
    withCompiledSchema(totoObjectSchema) { fun =>
      assertEquals(
        doApply(fun, parseJsonValue("""{"toto": 13,"titi": true}""")),
        BasicOutput(false, Seq(WithPointer(TypeMismatch("string"), Pointer.empty / "titi")))
      )
    }
  }

  test("object missing property") {
    withCompiledSchema(totoObjectSchema) { fun =>
      assertEquals(
        doApply(fun, parseJsonValue("""{"toto": 13}""")),
        BasicOutput(true, Seq())
      )
    }
  }

  test("object missing required property") {
    withCompiledSchema("""{
                         |"type": "object",
                         |"properties": {
                         |  "toto": { "type": "number" },
                         |  "titi": { "type": "string" }
                         |},
                         |"required": ["titi"]
                         |}
                         |""".stripMargin) { fun =>
      assertEquals(
        doApply(fun, parseJsonValue("""{"toto": 13}""")),
        BasicOutput(false, Seq(WithPointer(MissingRequiredProperties(Seq("titi")))))
      )
    }
  }

  test("all of") {
    withCompiledSchema(allOfSchema) { fun =>
      assertEquals(
        doApply(fun, parseJsonValue("13")),
        BasicOutput(true, Seq())
      )
    }
  }

  test("impossible all of") {
    withCompiledSchema("""{
                         |"allOf": [
                         |  { "type": "number" },
                         |  { "type": "string" }
                         |]
                         |}
                         |""".stripMargin) { fun =>
      assertEquals(
        doApply(fun, parseJsonValue("1313")),
        BasicOutput(false, Seq(WithPointer(TypeMismatch("string"))))
      )
    }
  }

  test("any of") {
    withCompiledSchema(anyOfSchema) { fun =>
      assertEquals(
        doApply(fun, parseJsonValue("1313")),
        BasicOutput(true, Seq())
      )
      assertEquals(
        doApply(fun, parseJsonValue("true")),
        BasicOutput(
          false,
          Seq(
            WithPointer(TypeMismatch("number")),
            WithPointer(TypeMismatch("string"))
          )
        )
      )
    }
  }

  test("one of") {
    withCompiledSchema(oneOfSchema) { fun =>
      assertEquals(
        doApply(fun, parseJsonValue("1313")),
        BasicOutput(true, Seq())
      )
    }
  }

  test("failed one of: none") {
    withCompiledSchema("""{
                         |"oneOf": [
                         |  { "type": "string" },
                         |  { "type": "boolean" }
                         |]
                         |}
                         |""".stripMargin) { fun =>
      assertEquals(
        doApply(fun, parseJsonValue("1313")),
        BasicOutput(false, Seq(WithPointer(TypeMismatch("string")), WithPointer(TypeMismatch("boolean"))))
      )
    }
  }

  test("failed one of: two") {
    withCompiledSchema("""{
                         |"oneOf": [
                         |  { "type": "number" },
                         |  { "type": "number" }
                         |]
                         |}
                         |""".stripMargin) { fun =>
      assertEquals(
        doApply(fun, parseJsonValue("1313")),
        BasicOutput(false, Seq(WithPointer(NotOneOf(2))))
      )
    }
  }

  test("not") {
    withCompiledSchema("""{"not": { "type": "number" }}""") { fun =>
      assertEquals(
        doApply(fun, parseJsonValue("true")),
        BasicOutput(true, Seq())
      )
      assertEquals(
        doApply(fun, parseJsonValue("1313")),
        BasicOutput(false, Seq(WithPointer(NotInvalid())))
      )
    }
  }

  test("if/then/else") {
    withCompiledSchema(ifThenElseSchema) { fun =>
      assertEquals(
        doApply(fun, parseJsonValue("1313")),
        BasicOutput(true, Seq())
      )
      assertEquals(
        doApply(fun, parseJsonValue(""""string"""")),
        BasicOutput(true, Seq())
      )
      assertEquals(
        doApply(fun, parseJsonValue("null")),
        BasicOutput(false, Seq(WithPointer(TypeMismatch("string"))))
      )
    }
  }

  test("if/else") {
    withCompiledSchema("""{
                         |"if": { "type": "number" },
                         |"else": { "type": "string" }
                         |}
                         |""".stripMargin) { fun =>
      assertEquals(
        doApply(fun, parseJsonValue("1313")),
        BasicOutput(true, Seq())
      )
      assertEquals(
        doApply(fun, parseJsonValue(""""string"""")),
        BasicOutput(true, Seq())
      )
      assertEquals(
        doApply(fun, parseJsonValue("null")),
        BasicOutput(false, Seq(WithPointer(TypeMismatch("string"))))
      )
    }
  }

  test("if/then") {
    withCompiledSchema("""{
                         |"if": { "type": "number" },
                         |"then": { "type": "number" }
                         |}
                         |""".stripMargin) { fun =>
      assertEquals(
        doApply(fun, parseJsonValue("1313")),
        BasicOutput(true, Seq())
      )
      assertEquals(
        doApply(fun, parseJsonValue(""""string"""")),
        BasicOutput(true, Seq())
      )
      assertEquals(
        doApply(fun, parseJsonValue("null")),
        BasicOutput(true, Seq())
      )
    }
  }

  test("then/else") {
    withCompiledSchema("""{
                         |"then": { "type": "number" },
                         |"else": { "type": "string" }
                         |}
                         |""".stripMargin) { fun =>
      assertEquals(
        doApply(fun, parseJsonValue("1313")),
        BasicOutput(true, Seq())
      )
      assertEquals(
        doApply(fun, parseJsonValue(""""string"""")),
        BasicOutput(true, Seq())
      )
      assertEquals(
        doApply(fun, parseJsonValue("null")),
        BasicOutput(true, Seq())
      )
    }
  }

  test("null or string") {
    withCompiledSchema(nullOrStringSchema) { fun =>
      assertEquals(
        doApply(fun, parseJsonValue("null")),
        BasicOutput(true, Seq())
      )
      assertEquals(
        doApply(fun, parseJsonValue(""""hello"""")),
        BasicOutput(true, Seq())
      )
      assertEquals(
        doApply(fun, parseJsonValue("13")),
        BasicOutput(false, Seq(WithPointer(TypeMismatch("null")), WithPointer(TypeMismatch("string"))))
      )
    }
  }

  test("enum") {
    withCompiledSchema(enumSchema) { fun =>
      assertEquals(
        doApply(fun, parseJsonValue(""""foo"""")),
        BasicOutput(true, Seq())
      )
      assertEquals(
        doApply(fun, parseJsonValue(""""bar"""")),
        BasicOutput(true, Seq())
      )
      assertEquals(
        doApply(fun, parseJsonValue(""""hello"""")),
        BasicOutput(
          false,
          Seq(
            WithPointer(NotInEnum(Seq(StringValue("foo"), StringValue("bar"))))
          )
        )
      )
    }
  }

  test("const") {
    withCompiledSchema(constSchema) { fun =>
      assertEquals(
        doApply(fun, parseJsonValue(""""first"""")),
        BasicOutput(true, Seq())
      )
      assertEquals(
        doApply(fun, parseJsonValue("{}")),
        BasicOutput(
          false,
          Seq(
            WithPointer(NotInEnum(Seq(StringValue("first")))),
            WithPointer(TypeMismatch("string"))
          )
        )
      )
      assertEquals(
        doApply(fun, parseJsonValue(""""second"""")),
        BasicOutput(
          false,
          Seq(
            WithPointer(NotInEnum(Seq(StringValue("first"))))
          )
        )
      )
    }
  }

  test("missing $id/$ref/$def") {
    withCompiledSchema(missingIdRefDefsSchema) { fun =>
      assertEquals(
        doApply(fun, parseJsonValue("[1313]")),
        BasicOutput(false, Seq(WithPointer(CannotResolve("#missing", None), Pointer.empty / 0)))
      )
    }
  }

  test("$id/$ref/$def") {
    withCompiledSchema(idRefDefsSchema) { fun =>
      assertEquals(
        doApply(fun, parseJsonValue("[1313]")),
        BasicOutput(true, Seq())
      )
      assertEquals(
        doApply(fun, parseJsonValue("""["hello"]""")),
        BasicOutput(false, Seq(WithPointer(TypeMismatch("number"), Pointer.empty / 0)))
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
        Seq(BasicOutput(true, Seq()), BasicOutput(false, Seq(WithPointer(TypeMismatch("number"), Pointer.empty / 0))))
      )
    }
  }

  test("$ref in properties") {
    withCompiledSchema(refInPropertiesSchema) { fun =>
      assertEquals(
        doApply(fun, parseJsonValue("""{ "foo": 13 }""")),
        BasicOutput(true, Seq())
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
          BasicOutput(true, Seq()),
          BasicOutput(true, Seq()),
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
          BasicOutput(true, Seq()),
          BasicOutput(true, Seq()),
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

  // done ValidationProcessingTest: all tests migrated

  // start ValidattionKeywordTest

  test("multipleOf") {
    withCompiledSchema("""{"multipleOf": 2}""") { fun =>
      assertEquals(
        doApplyBulk(
          fun,
          Seq(parseJsonValue("""13"""), parseJsonValue("""12""")),
          state => {}
        ),
        Seq(
          BasicOutput(
            false,
            Seq(
              WithPointer(NotMultipleOf(2))
            )
          ),
          BasicOutput(true, Seq())
        )
      )
    }
  }

  test("minimum") {
    withCompiledSchema("""{"minimum": 13}""") { fun =>
      assertEquals(
        doApplyBulk(
          fun,
          Seq(parseJsonValue("""12"""), parseJsonValue("""1313""")),
          state => {}
        ),
        Seq(
          BasicOutput(
            false,
            Seq(
              WithPointer(MinimumMismatch(13, exclude = false))
            )
          ),
          BasicOutput(true, Seq())
        )
      )
    }
  }

  test("exclusiveMinimum") {
    withCompiledSchema("""{"exclusiveMinimum": 13}""") { fun =>
      assertEquals(
        doApplyBulk(
          fun,
          Seq(parseJsonValue("""13"""), parseJsonValue("""14""")),
          state => {}
        ),
        Seq(
          BasicOutput(
            false,
            Seq(
              WithPointer(MinimumMismatch(13, exclude = true))
            )
          ),
          BasicOutput(true, Seq())
        )
      )
    }
  }

  test("maxLength") {
    withCompiledSchema("""{"maxLength": 3}""") { fun =>
      assertEquals(
        doApplyBulk(
          fun,
          Seq(
            parseJsonValue(""""toto""""),
            parseJsonValue(""""bar"""")
          ),
          state => {}
        ),
        Seq(
          BasicOutput(
            false,
            Seq(
              WithPointer(MaxLengthMismatch(3))
            )
          ),
          BasicOutput(true, Seq())
        )
      )
    }
  }

  test("minLength") {
    withCompiledSchema("""{"minLength": 4}""") { fun =>
      assertEquals(
        doApplyBulk(
          fun,
          Seq(
            parseJsonValue(""""bar""""),
            parseJsonValue(""""toto"""")
          ),
          state => {}
        ),
        Seq(
          BasicOutput(
            false,
            Seq(
              WithPointer(MinLengthMismatch(4))
            )
          ),
          BasicOutput(true, Seq())
        )
      )
    }
  }

  test("pattern") {
    withCompiledSchema("""{"pattern": "foo\\d\\d"}""") { fun =>
      assertEquals(
        doApplyBulk(
          fun,
          Seq(
            parseJsonValue(""""foo""""),
            parseJsonValue(""""foo13"""")
          ),
          state => {}
        ),
        Seq(
          BasicOutput(
            false,
            Seq(
              WithPointer(PatternMismatch("foo\\d\\d"))
            )
          ),
          BasicOutput(true, Seq())
        )
      )
    }
  }

  test("minItems") {
    withCompiledSchema("""{"minItems": 3}""") { fun =>
      assertEquals(
        doApplyBulk(
          fun,
          Seq(
            parseJsonValue("""[1,2]"""),
            parseJsonValue("""[2,3,4]""")
          ),
          state => {}
        ),
        Seq(
          BasicOutput(
            false,
            Seq(
              WithPointer(MinItemsMismatch(3))
            )
          ),
          BasicOutput(true, Seq())
        )
      )
    }
  }

  test("maxItems") {
    withCompiledSchema("""{"maxItems": 2}""") { fun =>
      assertEquals(
        doApplyBulk(
          fun,
          Seq(
            parseJsonValue("""[1,2,3]"""),
            parseJsonValue("""[2,3]""")
          ),
          state => {}
        ),
        Seq(
          BasicOutput(
            false,
            Seq(
              WithPointer(MaxItemsMismatch(2))
            )
          ),
          BasicOutput(true, Seq())
        )
      )
    }
  }

  test("uniqueItems") {
    withCompiledSchema("""{"uniqueItems": true}""") { fun =>
      assertEquals(
        doApplyBulk(
          fun,
          Seq(
            parseJsonValue("""[1,1]"""),
            parseJsonValue("""[13]""")
          ),
          state => {}
        ),
        Seq(
          BasicOutput(
            false,
            Seq(
              WithPointer(ItemsNotUnique())
            )
          ),
          BasicOutput(true, Seq())
        )
      )
    }
  }

  test("maxProperties") {
    withCompiledSchema("""{"maxProperties": 2}""") { fun =>
      assertEquals(
        doApplyBulk(
          fun,
          Seq(
            parseJsonValue("""{"gnu": 1, "bar": 2, "foo": 3}"""),
            parseJsonValue("""{"bar": 2, "foo": 3}""")
          ),
          state => {}
        ),
        Seq(
          BasicOutput(
            false,
            Seq(
              WithPointer(MaxPropertiesMismatch(2))
            )
          ),
          BasicOutput(true, Seq())
        )
      )
    }
  }

  test("minProperties") {
    withCompiledSchema("""{"minProperties": 3}""") { fun =>
      assertEquals(
        doApplyBulk(
          fun,
          Seq(
            parseJsonValue("""{"bar": 2, "foo": 3}"""),
            parseJsonValue("""{"gnu": 1, "bar": 2, "foo": 3}""")
          ),
          state => {}
        ),
        Seq(
          BasicOutput(
            false,
            Seq(
              WithPointer(MinPropertiesMismatch(3))
            )
          ),
          BasicOutput(true, Seq())
        )
      )
    }
  }

  test("required") {
    withCompiledSchema("""{"required": ["bar", "foo"]}""") { fun =>
      assertEquals(
        doApplyBulk(
          fun,
          Seq(
            parseJsonValue("""{"gnu": 1, "bar": 2}"""),
            parseJsonValue("""{"bar": 2, "foo": 3}""")
          ),
          state => {}
        ),
        Seq(
          BasicOutput(
            false,
            Seq(
              WithPointer(MissingRequiredProperties(Seq("foo")))
            )
          ),
          BasicOutput(true, Seq())
        )
      )
    }
  }

  test("dependentRequired") {
    withCompiledSchema("""{"dependentRequired": {"foo": ["bar", "gnu"]}}""") { fun =>
      assertEquals(
        doApplyBulk(
          fun,
          Seq(
            parseJsonValue("""{"foo": 1, "bar": 2}"""),
            parseJsonValue("""{"foo": 1, "bar": 2, "gnu": 3}""")
          ),
          state => {}
        ),
        Seq(
          BasicOutput(
            false,
            Seq(
              WithPointer(DependentRequiredMissing(Map("foo" -> Seq("gnu"))))
            )
          ),
          BasicOutput(true, Seq())
        )
      )
    }
  }

  test("dependentSchemas") {
    withCompiledSchema("""{"dependentSchemas": {"foo": true, "gnu": false}}""") { fun =>
      assertEquals(
        doApplyBulk(
          fun,
          Seq(
            parseJsonValue("""{"gnu": 1}"""),
            parseJsonValue("""{"foo": 1}""")
          ),
          state => {}
        ),
        Seq(
          BasicOutput(
            false,
            Seq(
              WithPointer(FalseSchemaReason())
            )
          ),
          BasicOutput(true, Seq())
        )
      )
    }
  }

  test("prefixItems") {
    withCompiledSchema("""{"prefixItems": [{"type": "number"}, {"type": "string"}]}""") { fun =>
      assertEquals(
        doApplyBulk(
          fun,
          Seq(
            parseJsonValue("""["gnu"]"""),
            parseJsonValue("""[13, "foo", true]""")
          ),
          state => {}
        ),
        Seq(
          BasicOutput(
            false,
            Seq(
              WithPointer(TypeMismatch("number"), Pointer.empty / 0)
            )
          ),
          BasicOutput(true, Seq())
        )
      )
    }
  }

  test("prefixItems and items") {
    withCompiledSchema("""|{"prefixItems": [{"type": "number"}, {"type": "string"}],
                          |"items": {"type": "boolean"}
                          |}""".stripMargin) { fun =>
      assertEquals(
        doApplyBulk(
          fun,
          Seq(
            parseJsonValue("""[13, "gnu", "boom"]"""),
            parseJsonValue("""[13, "foo", true]""")
          ),
          state => {}
        ),
        Seq(
          BasicOutput(
            false,
            Seq(
              WithPointer(TypeMismatch("boolean"), Pointer.empty / 2)
            )
          ),
          BasicOutput(true, Seq())
        )
      )
    }
  }

  test("contains") {
    withCompiledSchema("""{"contains": {"type": "number"}}""") { fun =>
      assertEquals(
        doApplyBulk(
          fun,
          Seq(
            parseJsonValue("""["gnu", true]"""),
            parseJsonValue("""[13, "foo", true]"""),
            parseJsonValue("""[13, 14, "foo", true]""")
          ),
          state => {}
        ),
        Seq(
          BasicOutput(
            false,
            Seq(
              WithPointer(NotContains(0))
            )
          ),
          BasicOutput(true, Seq()),
          BasicOutput(true, Seq())
        )
      )
    }
  }

  test("minContains") {
    withCompiledSchema("""|{"contains": {"type": "number"},
                          |"minContains": 2
                          |}""".stripMargin) { fun =>
      assertEquals(
        doApplyBulk(
          fun,
          Seq(
            parseJsonValue("""[13, "gnu", true]"""),
            parseJsonValue("""[13, 14, "foo", true]""")
          ),
          state => {}
        ),
        Seq(
          BasicOutput(
            false,
            Seq(
              WithPointer(NotContains(1))
            )
          ),
          BasicOutput(true, Seq())
        )
      )
    }
  }

  test("maxContains") {
    withCompiledSchema("""|{"contains": {"type": "number"},
                          |"maxContains": 2
                          |}""".stripMargin) { fun =>
      assertEquals(
        doApplyBulk(
          fun,
          Seq(
            parseJsonValue("""[13, 14, 15, "gnu", true]"""),
            parseJsonValue("""[]"""),
            parseJsonValue("""[13, 14, "foo", true]""")
          ),
          state => {}
        ),
        Seq(
          BasicOutput(
            false,
            Seq(
              WithPointer(NotContains(3))
            )
          ),
          BasicOutput(
            false,
            Seq(
              WithPointer(NotContains(0))
            )
          ),
          BasicOutput(true, Seq())
        )
      )
    }
  }

  test("minContains without contains") {
    withCompiledSchema("""{"minContains": 2}""") { fun =>
      assertEquals(
        doApplyBulk(
          fun,
          Seq(
            parseJsonValue("""[13, "gnu", true]"""),
            parseJsonValue("""[13, 14, "foo", true]""")
          ),
          state => {}
        ),
        Seq(
          BasicOutput(true, Seq()),
          BasicOutput(true, Seq())
        )
      )
    }
  }

  test("maxContains without contains") {
    withCompiledSchema("""{"maxContains": 2}""") { fun =>
      assertEquals(
        doApplyBulk(
          fun,
          Seq(
            parseJsonValue("""[13, "gnu", true]"""),
            parseJsonValue("""[13, 14, "foo", true]""")
          ),
          state => {}
        ),
        Seq(
          BasicOutput(true, Seq()),
          BasicOutput(true, Seq())
        )
      )
    }
  }

  test("patternProperties") {
    withCompiledSchema("""|{"patternProperties": { "^f": {} },
                          |"additionalProperties": false
                          |}""".stripMargin) { fun =>
      assertEquals(
        doApplyBulk(
          fun,
          Seq(
            parseJsonValue("""{"gnu": 13, "bar": true}"""),
            parseJsonValue("""{"foo": "ok"}""")
          ),
          state => {}
        ),
        Seq(
          BasicOutput(
            false,
            Seq(
              WithPointer(FalseSchemaReason(), Pointer.empty / "bar"),
              WithPointer(FalseSchemaReason(), Pointer.empty / "gnu")
            )
          ),
          BasicOutput(true, Seq())
        )
      )
    }
  }

  test("missing deep lazy $ref raises error") {
    withCompiledSchema("""|{
                          |"$id": "http://myhost:1313/",
                          |"items": {"$ref": "myItems"},
                          |"$defs": {
                          |  "foo": {
                          |    "$id": "myItems",
                          |    "$ref": "missing.json"
                          |  }
                          |}
                          |}
                          |""".stripMargin) { fun =>
      assertEquals(
        doApplyBulk(
          fun,
          Seq(
            parseJsonValue("""[ 13 ]""")
          ),
          state => {}
        ),
        Seq(
          BasicOutput(
            false,
            Seq(
              WithPointer(CannotResolve("missing.json", None), Pointer.empty / 0)
            )
          )
        )
      )
    }
  }

  test("unevaluatedItems") {
    withCompiledSchema("""{ "unevaluatedItems": { "type": "string" } }""") { fun =>
      assertEquals(
        doApplyBulk(
          fun,
          Seq(
            parseJsonValue("""[]"""),
            parseJsonValue("""["foo"]"""),
            parseJsonValue("""[42]""")
          ),
          state => {}
        ),
        Seq(
          BasicOutput(true, Seq()),
          BasicOutput(true, Seq()),
          BasicOutput(false, Seq(WithPointer(TypeMismatch("string"), Pointer.empty / 0)))
        )
      )
    }
  }
