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
import frawa.typedjson.keywords.WithPointer

class SamplesTest extends FunSuite:
  given Parser = new JawnParser()

  import frawa.typedjson.eval.FlagOutput.given

  test("always valid without a schema") {
    val typedJson = TypedJson.create()
    val json      = """{"foo":"bar"}"""
    val result    = typedJson.eval(json)
    assertEquals(result.map(_.valid), Right(true))
  }

  test("use schema to validate several values") {
    val schemaJson = """{"type": "string"}"""
    val typedJson  = TypedJson.create(schemaJson).toOption.get

    val validJson = """"foo""""
    assertEquals(typedJson.eval(validJson).map(_.valid), Right(true))
    val invalidJson = """13"""
    assertEquals(typedJson.eval(invalidJson).map(_.valid), Right(false))
  }

  test("obtain validation errors") {
    import frawa.typedjson.eval.BasicOutput
    import frawa.typedjson.eval.BasicOutput.given

    val schemaJson = """{"type": "string"}"""
    val typedJson  = TypedJson.create(schemaJson).toOption.get

    val invalidJson = """true"""
    val validation  = typedJson.eval(invalidJson)
    assertEquals(validation.map(_.valid), Right(false))
    assertEquals(validation.map(_.errors), Right(Seq(WithPointer(TypeMismatch("string")))))
  }
