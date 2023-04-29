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

package samples

import munit.FunSuite

import frawa.typedjson.TypedJson
import frawa.typedjson.parser.jawn.JawnParser
import frawa.typedjson.validation.TypeMismatch
import frawa.typedjson.pointer.Pointer
import frawa.typedjson.parser.Parser
import frawa.typedjson.util.WithPointer
import frawa.typedjson.macros.JsonUtils.parseJsonValue

class SamplesTest extends FunSuite:
  given Parser = new JawnParser()

  import frawa.typedjson.output.FlagOutput.given

  test("no output without a schema") {
    val typedJson = TypedJson.create()
    val json      = parseJsonValue("""{"foo":"bar"}""")
    val (o, _)    = typedJson.eval(json)
    assertEquals(o, None)
  }

  test("use schema to validate several values") {
    val schemaJson = """{"type": "string"}"""
    val typedJson  = TypedJson.create(schemaJson).toOption.get

    val validJson       = parseJsonValue(""""foo"""")
    val (o, typedJson1) = typedJson.eval(validJson)
    assertEquals(o.map(_.valid), Some(true))
    val invalidJson = parseJsonValue("""13""")
    val (o2, _)     = typedJson1.eval(invalidJson)
    assertEquals(o2.map(_.valid), Some(false))
  }

  test("use schema to validate several values at once") {
    val schemaJson = """{"type": "string"}"""
    val typedJson  = TypedJson.create(schemaJson).toOption.get

    val validJson   = parseJsonValue(""""foo"""")
    val invalidJson = parseJsonValue("""13""")

    val (os, _) = typedJson.evalBulk(Seq(validJson, invalidJson))
    assertEquals(os.map(_.valid), Seq(true, false))
  }

  test("obtain validation errors") {
    import frawa.typedjson.output.BasicOutput
    import frawa.typedjson.output.BasicOutput.given

    val schemaJson = """{"type": "string"}"""
    val typedJson  = TypedJson.create(schemaJson).toOption.get

    val invalidJson = parseJsonValue("""true""")
    val (o, _)      = typedJson.eval(invalidJson)
    assertEquals(o.map(_.valid), Some(false))
    assertEquals(o.map(_.errors), Some(Seq(WithPointer(TypeMismatch("string")))))
  }
