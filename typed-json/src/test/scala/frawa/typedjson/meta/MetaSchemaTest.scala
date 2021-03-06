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

import frawa.typedjson.keywords._
import frawa.typedjson.testutil.EvaluatorFactory
import frawa.typedjson.testutil.TestUtil._
import frawa.typedjson.validation.{ValidationProcessing, ValidationOutput}
import munit.FunSuite

// TODO still needed?
class MetaSchemaTest extends FunSuite {

  private val resolver                                               = MetaSchemas.lazyResolver
  private val base                                                   = MetaSchemas.draft202012
  private val lazyResolver: Some[LoadedSchemasResolver.LazyResolver] = Some(MetaSchemas.lazyResolver)

  implicit val factory: EvaluatorFactory[SchemaValue, ValidationOutput] =
    EvaluatorFactory.make(ValidationProcessing(), lazyResolver = lazyResolver)

  def withSchemaSpec(name: String)(f: SchemaValue => Unit): Unit = {
    val Some(schema) = resolver(base.resolve(name))
    f(schema)
  }

  def validateSpec(valueName: String, schemaName: String)(f: Result[ValidationOutput] => Unit): Unit = {
    withSchemaSpec(schemaName) { schema =>
      withSchemaSpec(valueName) { value =>
        withProcessor[ValidationOutput](schema) { evaluator =>
          val result = evaluator(InnerValue(value.value))
          f(result)
        }
      }
    }
  }

  test("validate core against core") {
    validateSpec("meta/core", "meta/core") { result =>
      assertEquals(result.output, None)
      // assertEquals(result.count, 54)
      // assertEquals(result.count, 42)
      assertEquals(result.count, 22)
      assertEquals(
        result.ignoredKeywords(),
        Set(
          "properties",
          "title",
          "type"
        )
      )
    }
  }

  test("validate core against validation") {
    validateSpec("meta/core", "meta/validation") { result =>
      assertEquals(result.output, None)
      // assertEquals(result.count, 22)
      assertEquals(result.count, 12)
      assertEquals(
        result.ignoredKeywords(),
        Set.empty[String]
      )
    }
  }

  test("validate core against applicator") {
    validateSpec("meta/core", "meta/applicator") { result =>
      assertEquals(result.output, None)
      // assertEquals(result.count, 84)
      // assertEquals(result.count, 48)
      assertEquals(result.count, 18)
      assertEquals(
        result.ignoredKeywords(),
        Set(
          "type",
          "default",
          "title"
        )
      )
    }
  }

  test("validate validation against core") {
    validateSpec("meta/validation", "meta/core") { result =>
      assertEquals(result.output, None)
      // assertEquals(result.count, 65)
      // assertEquals(result.count, 44)
      assertEquals(result.count, 23)
      assertEquals(
        result.ignoredKeywords(),
        Set(
          "properties",
          "title",
          "type"
        )
      )
    }
  }

  test("validate validation against validation") {
    validateSpec("meta/validation", "meta/validation") { result =>
      assertEquals(result.output, None)
      // assertEquals(result.count, 22)
      assertEquals(result.count, 12)
      assertEquals(
        result.ignoredKeywords(),
        Set.empty[String]
      )
    }
  }

  test("validate validation against applicator") {
    validateSpec("meta/validation", "meta/applicator") { result =>
      assertEquals(result.output, None)
      // assertEquals(result.count, 168)
      // assertEquals(result.count, 91)
      assertEquals(result.count, 32)
      assertEquals(
        result.ignoredKeywords(),
        Set(
          "type",
          "default",
          "title",
          "minItems"
        )
      )
    }
  }
}
