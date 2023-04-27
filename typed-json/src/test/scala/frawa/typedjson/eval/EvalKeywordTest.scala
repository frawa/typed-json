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

class EvalKeywordTest extends FunSuite:

  import Util.*

  private val evalBasic        = Eval[MyR, BasicOutput]
  given Eval[MyR, BasicOutput] = evalBasic

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
          BasicOutput(true)
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
          BasicOutput(true)
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
          BasicOutput(true)
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
          BasicOutput(true)
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
          BasicOutput(true)
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
          BasicOutput(true)
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
          BasicOutput(true)
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
          BasicOutput(true)
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
          BasicOutput(true)
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
          BasicOutput(true)
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
          BasicOutput(true)
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
          BasicOutput(true)
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
          BasicOutput(true)
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
          BasicOutput(true)
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
          BasicOutput(true, annotations = List(EvaluatedIndices(Seq(0, 1))))
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
            ),
            annotations = Seq(EvaluatedIndices(Seq(0, 1)))
          ),
          BasicOutput(true, annotations = Seq(EvaluatedIndices(Seq(0, 1, 2))))
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
          BasicOutput(true, annotations = List(EvaluatedIndices(Seq(0)))),
          BasicOutput(true, annotations = List(EvaluatedIndices(Seq(0, 1))))
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
            ),
            annotations = Seq(EvaluatedIndices(Seq(0)))
          ),
          BasicOutput(true, annotations = Seq(EvaluatedIndices(Seq(0, 1))))
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
            ),
            annotations = Seq(EvaluatedIndices(Seq(0, 1, 2)))
          ),
          BasicOutput(
            false,
            Seq(
              WithPointer(NotContains(0))
            )
          ),
          BasicOutput(true, annotations = Seq(EvaluatedIndices(Seq(0, 1))))
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
          BasicOutput(true),
          BasicOutput(true)
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
          BasicOutput(true),
          BasicOutput(true)
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
          BasicOutput(true, annotations = Seq(EvaluatedProperties(Set("foo"))))
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
          BasicOutput(true),
          BasicOutput(true),
          BasicOutput(false, Seq(WithPointer(TypeMismatch("string"), Pointer.empty / 0)))
        )
      )
    }
  }

  test("unevaluatedItems with items") {
    withCompiledSchema("""{ "items": { "type": "boolean" }, "unevaluatedItems": { "type": "string" } }""") { fun =>
      assertEquals(
        doApplyBulk(
          fun,
          Seq(
            parseJsonValue("""[true, "foo", false]"""),
            parseJsonValue("""[true, 13, false]""")
          ),
          state => {}
        ),
        Seq(
          BasicOutput(true),
          BasicOutput(false, Seq(WithPointer(TypeMismatch("string"), Pointer.empty / 1)))
        )
      )
    }
  }

  test("unevaluatedItems with contains") {
    withCompiledSchema("""{ "unevaluatedItems": { "type": "string" }, "contains": {"type": "boolean"}  }""") { fun =>
      assertEquals(
        doApplyBulk(
          fun,
          Seq(
            parseJsonValue("""[true, "foo", false]"""),
            parseJsonValue("""[true, 13, false]""")
          ),
          state => {}
        ),
        Seq(
          BasicOutput(true),
          BasicOutput(false, Seq(WithPointer(TypeMismatch("string"), Pointer.empty / 1)))
        )
      )
    }
  }

  test("unevaluatedItems with items and contains") {
    withCompiledSchema("""|{
                          |"items": {"type": "array"},
                          |"unevaluatedItems": { "type": "string" }, 
                          |"contains": {"type": "boolean"}  
                          |}""".stripMargin) { fun =>
      assertEquals(
        doApplyBulk(
          fun,
          Seq(
            parseJsonValue("""[true, "foo", false, [13], [14]]"""),
            parseJsonValue("""[true, 13, false, [13]]""")
          ),
          state => {}
        ),
        Seq(
          BasicOutput(true),
          BasicOutput(false, Seq(WithPointer(TypeMismatch("string"), Pointer.empty / 1)))
        )
      )
    }
  }

  test("unevaluatedItems with nested items") {
    withCompiledSchema("""|{
                          |"allOf": [{"items": {"type": "boolean"}}],
                          |"unevaluatedItems": { "type": "string" }
                          |}""".stripMargin) { fun =>
      assertEquals(
        doApplyBulk(
          fun,
          Seq(
            parseJsonValue("""[true, "foo", false]"""),
            parseJsonValue("""[true, 13, false]""")
          ),
          state => {}
        ),
        Seq(
          BasicOutput(true),
          BasicOutput(false, Seq(WithPointer(TypeMismatch("string"), Pointer.empty / 1)))
        )
      )
    }
  }

  test("unevaluatedProperties") {
    withCompiledSchema("""|{
                          |"properties": {"foo": {"type": "string"}},
                          |"unevaluatedProperties": { "type": "number" }
                          |}""".stripMargin) { fun =>
      assertEquals(
        doApplyBulk(
          fun,
          Seq(
            parseJsonValue("""{"foo":"bar", "gnu":13}"""),
            parseJsonValue("""{"foo":"bar", "gnu":true}""")
          ),
          state => {}
        ),
        Seq(
          BasicOutput(true),
          BasicOutput(false, Seq(WithPointer(TypeMismatch("number"), Pointer.empty / "gnu")))
        )
      )
    }
  }
