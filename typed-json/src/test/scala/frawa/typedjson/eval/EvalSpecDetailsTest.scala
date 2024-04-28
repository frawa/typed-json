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
import frawa.typedjson.testsuite.Remotes
import frawa.typedjson.keywords.EvaluatedIndices
import frawa.typedjson.keywords.EvaluatedProperties
import frawa.typedjson.keywords.Ignored
import frawa.typedjson.meta.MetaSchemas
import frawa.typedjson.output.FlagOutput
import frawa.typedjson.output.FlagOutput.given
import frawa.typedjson.output.SimpleOutput
import frawa.typedjson.output.SimpleOutput.given
import frawa.typedjson.pointer.Pointer
import frawa.typedjson.testutil.TestUtil.{_, given}
import frawa.typedjson.util.WithPointer
import frawa.typedjson.validation._
import munit.FunSuite

import java.net.URI

class EvalSpecDetailsTest extends FunSuite:

  import Util.*

  private val evalBasic       = Eval[R, SimpleOutput]
  given Eval[R, SimpleOutput] = evalBasic

  Eval[R, FlagOutput]

  test("neither oneOf valid (complex)") {
    withCompiledSchema("""|{
                          |            "$schema": "https://json-schema.org/draft/2020-12/schema",
                          |            "oneOf": [
                          |                {
                          |                    "properties": {
                          |                        "bar": {"type": "integer"}
                          |                    },
                          |                    "required": ["bar"]
                          |                },
                          |                {
                          |                    "properties": {
                          |                        "foo": {"type": "string"}
                          |                    },
                          |                    "required": ["foo"]
                          |                }
                          |            ]
                          |}""".stripMargin) { fun =>
      assertEquals(
        doApply(fun, parseJsonValue("""{"foo": 2, "bar": "quux"}""")),
        SimpleOutput(
          false,
          Seq(
            WithPointer(TypeMismatch("integer"), Pointer.empty / "bar"),
            WithPointer(TypeMismatch("string"), Pointer.empty / "foo")
          )
        )
      )
    }
  }

  test("an invalid due to the other is invalid") {
    withCompiledSchema("""|{
                          |            "$schema": "https://json-schema.org/draft/2020-12/schema",
                          |            "patternProperties": {
                          |                "a*": {"type": "integer"},
                          |                "aaa*": {"maximum": 20}
                          |            }
                          |}""".stripMargin) { fun =>
      assertEquals(
        doApply(fun, parseJsonValue("""{"aaaa": 31}""")),
        SimpleOutput(
          false,
          Seq(
            WithPointer(
              value = MaximumMismatch(20, false),
              Pointer.empty / "aaaa"
            )
          )
        )
      )
    }
  }

  test("patternProperty invalidates property") {
    withCompiledSchema("""|{
                          |            "$schema": "https://json-schema.org/draft/2020-12/schema",
                          |            "properties": {
                          |                "foo": {"type": "array", "maxItems": 3},
                          |                "bar": {"type": "array"}
                          |            },
                          |            "patternProperties": {"f.o": {"minItems": 2}},
                          |            "additionalProperties": {"type": "integer"}
                          |}""".stripMargin) { fun =>
      assertEquals(
        doApply(fun, parseJsonValue("""{"foo": []}""")),
        SimpleOutput(
          false,
          Seq(
            WithPointer(
              value = MinItemsMismatch(2, 0),
              Pointer.empty / "foo"
            )
          )
        )
      )
    }
  }

  test("with nested unevaluated properties") {
    withCompiledSchema("""|{
                          |            "$schema": "https://json-schema.org/draft/2020-12/schema",
                          |            "type": "object",
                          |            "properties": {
                          |                "foo": { "type": "string" }
                          |            },
                          |            "allOf": [
                          |                {
                          |                    "unevaluatedProperties": true
                          |                }
                          |            ],
                          |            "unevaluatedProperties": {
                          |                "type": "string",
                          |                "maxLength": 2
                          |            }
        }""".stripMargin) { fun =>
      assertEquals(
        doApply(
          fun,
          parseJsonValue("""|{
                            |    "foo": "foo",
                            |    "bar": "bar"
                            |}""".stripMargin)
        ),
        SimpleOutput(
          true,
          annotations = Seq(
            EvaluatedProperties(
              properties = Set(
                "bar",
                "foo"
              )
            )
          )
        )
      )
    }
  }

  test("when two match and has no unevaluated properties") {
    withCompiledSchema("""|{
                          |            "$schema": "https://json-schema.org/draft/2020-12/schema",
                          |            "type": "object",
                          |            "properties": {
                          |                "foo": { "type": "string" }
                          |            },
                          |            "anyOf": [
                          |                {
                          |                    "properties": {
                          |                        "bar": { "const": "bar" }
                          |                    },
                          |                    "required": ["bar"]
                          |                },
                          |                {
                          |                    "properties": {
                          |                        "baz": { "const": "baz" }
                          |                    },
                          |                    "required": ["baz"]
                          |                },
                          |                {
                          |                    "properties": {
                          |                        "quux": { "const": "quux" }
                          |                    },
                          |                    "required": ["quux"]
                          |                }
                          |            ],
                          |            "unevaluatedProperties": false
                          |}""".stripMargin) { fun =>
      assertEquals(
        doApply(
          fun,
          parseJsonValue("""|{
                            |                    "foo": "foo",
                            |                    "bar": "bar",
                            |                    "baz": "baz"
                            |}""".stripMargin)
        ),
        SimpleOutput(
          true,
          annotations = Seq(
            EvaluatedProperties(
              properties = Set(
                "bar",
                "baz",
                "foo"
              )
            )
          )
        )
      )
    }
  }

  test("unevaluatedItems with nested unevaluatedItems") {
    withCompiledSchema("""|{
                          |            "$schema": "https://json-schema.org/draft/2020-12/schema",
                          |            "allOf": [
                          |                {
                          |                    "prefixItems": [
                          |                        { "type": "string" }
                          |                    ]
                          |                },
                          |                { "unevaluatedItems": true }
                          |            ],
                          |            "unevaluatedItems": false
                          |}""".stripMargin) { fun =>
      assertEquals(
        doApply(
          fun,
          parseJsonValue("""["foo", 42, true]""")
        ),
        SimpleOutput(
          true,
          annotations = Seq(
            EvaluatedIndices(Set(0, 1, 2))
          )
        )
      )
    }
  }

  test("unevaluatedProperties with not") {
    withCompiledSchema("""|{
                          |            "$schema": "https://json-schema.org/draft/2020-12/schema",
                          |            "type": "object",
                          |            "properties": {
                          |                "foo": { "type": "string" }
                          |            },
                          |            "not": {
                          |                "not": {
                          |                    "properties": {
                          |                        "bar": { "const": "bar" }
                          |                    },
                          |                    "required": ["bar"]
                          |                }
                          |            },
                          |            "unevaluatedProperties": false
                          |}""".stripMargin) { fun =>
      assertEquals(
        doApply(
          fun,
          parseJsonValue("""|{
                            |                    "foo": "foo",
                            |                    "bar": "bar"
                            |}""".stripMargin)
        ),
        SimpleOutput(
          false,
          errors = List(
            WithPointer(
              value = FalseSchemaReason(),
              Pointer.empty / "bar"
            )
          )
        )
      )
    }
  }

  test("nested unevaluatedProperties, outer true, inner false, properties outside") {
    withCompiledSchema("""|{
                          |            "$schema": "https://json-schema.org/draft/2020-12/schema",
                          |            "type": "object",
                          |            "properties": {
                          |                "foo": { "type": "string" }
                          |            },
                          |            "allOf": [
                          |                {
                          |                    "unevaluatedProperties": false
                          |                }
                          |            ],
                          |            "unevaluatedProperties": true
                          |}""".stripMargin) { fun =>
      assertEquals(
        doApplyBulk(
          fun,
          Seq(
            parseJsonValue("""|{
                              |                    "foo": "foo"
                              |}""".stripMargin),
            parseJsonValue("""|{
                              |                    "foo": "foo",
                              |                    "bar": "bar"
                              |}""".stripMargin)
          ),
          { _ => }
        ),
        Seq(
          SimpleOutput(
            false,
            errors = Seq(
              WithPointer(
                FalseSchemaReason(),
                Pointer.empty / "foo"
              )
            )
          ),
          SimpleOutput(
            false,
            errors = Seq(
              WithPointer(
                FalseSchemaReason(),
                Pointer.empty / "bar"
              ),
              WithPointer(
                FalseSchemaReason(),
                Pointer.empty / "foo"
              )
            )
          )
        )
      )
    }
  }

  test("unevaluatedItems and contains interact to control item dependency relationship") {
    withCompiledSchema("""|{
                          |            "$schema": "https://json-schema.org/draft/2020-12/schema",
                          |            "if": {
                          |                "contains": {"const": "a"}
                          |            },
                          |            "then": {
                          |                "if": {
                          |                    "contains": {"const": "b"}
                          |                },
                          |                "then": {
                          |                    "if": {
                          |                        "contains": {"const": "c"}
                          |                    }
                          |                }
                          |            },
                          |            "unevaluatedItems": false
                          |}""".stripMargin) { fun =>
      assertEquals(
        doApplyBulk(
          fun,
          Seq(
            parseJsonValue("""[ "a", "b", "a", "b", "a" ]"""),
            parseJsonValue("""[ "c", "a", "c", "c", "b", "a" ]""")
          ),
          { _ => }
        ),
        Seq(
          SimpleOutput(
            true,
            annotations = Seq(EvaluatedIndices(Set(0, 1, 2, 3, 4)))
          ),
          SimpleOutput(
            true,
            annotations = Seq(EvaluatedIndices(Set(0, 1, 2, 3, 4, 5)))
          )
        )
      )
    }
  }

  test("Location-independent identifier in remote ref") {
    val lazyResolver = (uri: URI) => MetaSchemas.lazyResolver(uri).orElse(Remotes.lazyResolver(uri))
    withCompiledSchema(
      """|{
         |            "$schema": "https://json-schema.org/draft/2020-12/schema",
         |            "$ref": "http://localhost:1234/draft2020-12/locationIndependentIdentifier.json#/$defs/refToInteger"
         |}""".stripMargin,
      Some(lazyResolver)
    ) { fun =>
      assertEquals(
        doApplyBulk(
          fun,
          Seq(
            parseJsonValue("""1"""),
            parseJsonValue(""""foo"""")
          ),
          { _ => }
        ),
        Seq(
          SimpleOutput(
            true
          ),
          SimpleOutput(
            false,
            Seq(WithPointer(TypeMismatch("integer")))
          )
        )
      )
    }
  }

  test("remote HTTP ref with different $id") {
    val lazyResolver = (uri: URI) => Remotes.lazyResolver(uri)
    withCompiledSchema(
      """{"$ref": "http://localhost:1234/different-id-ref-string.json"}""",
      Some(lazyResolver)
    ) { fun =>
      assertEquals(
        doApplyBulk(
          fun,
          Seq(
            parseJsonValue("""1"""),
            parseJsonValue(""""foo"""")
          ),
          { _ => }
        ),
        Seq(
          SimpleOutput(
            false,
            Seq(WithPointer(TypeMismatch("string")))
          ),
          SimpleOutput(
            true
          )
        )
      )
    }
  }

  test("multiple dynamic paths to the $dynamicRef keyword") {
    val lazyResolver = (uri: URI) => Remotes.lazyResolver(uri)
    withCompiledSchema(
      """|{
         |            "$schema": "https://json-schema.org/draft/2020-12/schema",
         |            "$id": "https://test.json-schema.org/dynamic-ref-with-multiple-paths/main",
         |            "$defs": {
         |                "inner": {
         |                    "$id": "inner",
         |                    "$dynamicAnchor": "foo",
         |                    "title": "inner",
         |                    "additionalProperties": {
         |                        "$dynamicRef": "#foo"
         |                    }
         |                }
         |            },
         |            "if": {
         |                "propertyNames": {
         |                    "pattern": "^[a-m]"
         |                }
         |            },
         |            "then": {
         |                "title": "any type of node",
         |                "$id": "anyLeafNode",
         |                "$dynamicAnchor": "foo",
         |                "$ref": "inner"
         |            },
         |            "else": {
         |                "title": "integer node",
         |                "$id": "integerNode",
         |                "$dynamicAnchor": "foo",
         |                "type": [ "object", "integer" ],
         |                "$ref": "inner"
         |            }
         |}""".stripMargin,
      Some(lazyResolver)
    ) { fun =>
      assertEquals(
        doApplyBulk(
          fun,
          Seq(
            parseJsonValue("""{ "alpha": 1.1 }"""),
            parseJsonValue("""{ "november": 1.1 }""")
          ),
          { _ => }
        ),
        Seq(
          SimpleOutput(
            true,
            annotations = Seq(EvaluatedProperties(Set("alpha")), Ignored(Set("title")))
          ),
          SimpleOutput(
            false,
            errors = Seq(
              WithPointer(AdditionalPropertyInvalid("november"), Pointer.empty / "november"),
              WithPointer(TypeMismatch("object"), Pointer.empty / "november"),
              WithPointer(TypeMismatch("integer"), Pointer.empty / "november")
            )
          )
        )
      )
    }
  }

  test("root ref in remote ref") {
    val lazyResolver = (uri: URI) => Remotes.lazyResolver(uri)
    withCompiledSchema(
      """|{
         |            "$schema": "https://json-schema.org/draft/2020-12/schema",
         |            "$id": "http://localhost:1234/draft2020-12/object",
         |            "type": "object",
         |            "properties": {
         |                "name": {"$ref": "name-defs.json#/$defs/orNull"}
         |            }
         |}""".stripMargin,
      Some(lazyResolver)
    ) { fun =>
      assertEquals(
        doApplyBulk(
          fun,
          Seq(
            parseJsonValue("""{ "name": "foo" }"""),
            parseJsonValue("""{ "name": { "name": null } }""")
          ),
          { _ => }
        ),
        Seq(
          SimpleOutput(true, annotations = Seq(EvaluatedProperties(Set("name")))),
          SimpleOutput(
            false,
            errors = Seq(
              WithPointer(TypeMismatch("null"), Pointer.empty / "name"),
              WithPointer(TypeMismatch("string"), Pointer.empty / "name")
            )
          )
        )
      )
    }
  }

  test("anchor within remote ref") {
    val lazyResolver = (uri: URI) => Remotes.lazyResolver(uri)
    withCompiledSchema(
      """|{
         |            "$ref": "http://localhost:1234/draft2020-12/locationIndependentIdentifier.json#foo"
         |}""".stripMargin,
      Some(lazyResolver)
    ) { fun =>
      assertEquals(
        doApplyBulk(
          fun,
          Seq(
            parseJsonValue("""1"""),
            parseJsonValue(""""a"""")
          ),
          { _ => }
        ),
        Seq(
          SimpleOutput(true),
          SimpleOutput(
            false,
            errors = Seq(
              WithPointer(TypeMismatch("integer"))
            )
          )
        )
      )
    }
  }
