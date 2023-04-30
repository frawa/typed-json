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
        }""".stripMargin) { fun =>
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
