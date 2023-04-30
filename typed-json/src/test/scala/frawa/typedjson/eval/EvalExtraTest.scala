package frawa.typedjson.eval

import frawa.typedjson.eval.CacheState.R
import frawa.typedjson.keywords.{EvaluatedIndices, EvaluatedProperties}
import frawa.typedjson.parser.Value.{BoolValue, NullValue, StringValue}
import frawa.typedjson.pointer.Pointer
import frawa.typedjson.testutil.TestSchemas.*
import frawa.typedjson.testutil.TestUtil.{*, given}
import frawa.typedjson.validation.*
import munit.FunSuite
import frawa.typedjson.eval.CacheState
import frawa.typedjson.output.BasicOutput
import frawa.typedjson.output.BasicOutput.given
import frawa.typedjson.output.FlagOutput
import frawa.typedjson.output.FlagOutput.given
import frawa.typedjson.util.WithPointer
import frawa.typedjson.pointer.Pointer.parse

class EvalExtraTest extends FunSuite:

  import Util.*

  private val evalBasic      = Eval[R, BasicOutput]
  given Eval[R, BasicOutput] = evalBasic

  private val evalFlag = Eval[R, FlagOutput]

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
        BasicOutput(
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
        BasicOutput(
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
        BasicOutput(
          false,
          Seq(
            WithPointer(
              value = MinItemsMismatch(2),
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
        BasicOutput(
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
        BasicOutput(
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
          parseJsonValue("""["foo", 42, true]""".stripMargin)
        ),
        BasicOutput(
          true,
          annotations = Seq(
            EvaluatedIndices(Seq(0, 1, 2))
          )
        )
      )
    }
  }
