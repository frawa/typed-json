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

package frawa.typedjson.meta

import frawa.typedjson.eval.Eval
import frawa.typedjson.eval.CacheState.R
import frawa.typedjson.eval.Util.{doApply, withCompiledSchemaValue}
import frawa.typedjson.keywords.*
import frawa.typedjson.testutil.TestUtil.*
import munit.FunSuite
import frawa.typedjson.eval.CacheState
import frawa.typedjson.output.SimpleOutput
import frawa.typedjson.output.SimpleOutput.given
import frawa.typedjson.eval.Util.doApplyWithStats
import frawa.typedjson.output.OutputOps

class MetaSchemaTest extends FunSuite:

  private val resolver = MetaSchemas.lazyResolver
  private val base     = MetaSchemas.draft202012

  def withSchemaSpec(name: String)(f: SchemaValue => Unit): Unit =
    val Some(schema) = resolver(base.resolve(name)): @unchecked
    f(schema)

  def validateSpec(valueName: String, schemaName: String)(f: SimpleOutput => Unit, g: CacheState.Stats => Unit): Unit =
    val evalBasic               = Eval[R, SimpleOutput]
    given Eval[R, SimpleOutput] = evalBasic
    withSchemaSpec(schemaName) { schema =>
      withSchemaSpec(valueName) { value =>
        withCompiledSchemaValue(schema) { fun =>
          val (output, stats) = doApplyWithStats(fun, value.value)
          assertEquals(output.isValid, true)
          f(output)
          g(stats)
        }
      }
    }

  test("validate core against core") {
    validateSpec("meta/core", "meta/core")(
      { output =>
        assertEquals(
          OutputOps.ignoredKeywords(output.annotations),
          Set(
            "properties",
            "type",
            "title"
          )
        )
      },
      { stats =>
        assertEquals(
          stats,
          CacheState.Stats(
            binds = 11,
            hits = 0,
            cached = 0
          )
        )
      }
    )
  }

  test("validate core against validation") {
    validateSpec("meta/core", "meta/validation")(
      { output =>
        assertEquals(
          OutputOps.ignoredKeywords(output.annotations),
          Set(
            "properties",
            "title"
          )
        )
      },
      { stats =>
        assertEquals(
          stats,
          CacheState.Stats(
            binds = 18,
            hits = 0,
            cached = 0
          )
        )
      }
    )
  }

  test("validate core against applicator") {
    validateSpec("meta/core", "meta/applicator")(
      { output =>
        assertEquals(
          OutputOps.ignoredKeywords(output.annotations),
          Set(
            "type",
            "title"
          )
        )
      },
      { stats =>
        assertEquals(
          stats,
          CacheState.Stats(
            binds = 273,
            hits = 0,
            cached = 0
          )
        )
      }
    )
  }

  test("validate validation against core") {
    validateSpec("meta/validation", "meta/core")(
      { output =>
        assertEquals(
          OutputOps.ignoredKeywords(output.annotations),
          Set(
            "properties",
            "type",
            "title"
          )
        )
      },
      { stats =>
        assertEquals(
          stats,
          CacheState.Stats(
            binds = 11,
            hits = 0,
            cached = 0
          )
        )
      }
    )
  }

  test("validate validation against validation") {
    validateSpec("meta/validation", "meta/validation")(
      { output =>
        assertEquals(
          OutputOps.ignoredKeywords(output.annotations),
          Set(
            "properties",
            "title"
          )
        )
      },
      { stats =>
        assertEquals(
          stats,
          CacheState.Stats(
            binds = 18,
            hits = 0,
            cached = 0
          )
        )
      }
    )
  }

  test("validate validation against applicator") {
    validateSpec("meta/validation", "meta/applicator")(
      { output =>
        assertEquals(
          OutputOps.ignoredKeywords(output.annotations),
          Set(
            "type",
            "title"
          )
        )
      },
      { stats =>
        assertEquals(
          stats,
          CacheState.Stats(
            binds = 557,
            hits = 0,
            cached = 1
          )
        )
      }
    )
  }
