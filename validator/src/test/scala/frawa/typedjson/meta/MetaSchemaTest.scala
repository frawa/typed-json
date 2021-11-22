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

import munit.FunSuite
import frawa.typedjson.processor.SchemaValue
import frawa.typedjson.parser.ZioParser
import frawa.typedjson.processor.Result
import frawa.typedjson.processor.LoadedSchemasResolver
import frawa.typedjson.processor.InnerValue
import frawa.typedjson.testutil.TestUtil._
import frawa.typedjson.validation.{ValidationChecker, ValidationResult}

class MetaSchemaTest extends FunSuite {
  implicit val zioParser: ZioParser = new ZioParser()

  val resolver                                               = MetaSchemas.lazyResolver
  val base                                                   = MetaSchemas.draft202012
  val lazyResolver: Some[LoadedSchemasResolver.LazyResolver] = Some(MetaSchemas.lazyResolver)

  def withSchemaSpec(name: String)(f: SchemaValue => Unit): Unit = {
    val Some(schema) = resolver(base.resolve(name))
    f(schema)
  }

  def validateSpec(valueName: String, schemaName: String)(f: Result[ValidationResult] => Unit): Unit = {
    withSchemaSpec(schemaName) { schema =>
      withSchemaSpec(valueName) { value =>
        withProcessor(ValidationChecker())(schema, lazyResolver) { processor =>
          val result = processor(InnerValue(value.value))
          f(result)
        }
      }
    }
  }

  test("validate core against core") {
    validateSpec("meta/core", "meta/core") { result =>
      assertEquals(result.results, Seq())
      assertEquals(result.count, 54)
      assertEquals(
        result.problems.ignoredKeywords,
        Set("$vocabulary", "$schema")
      )
    }
  }

  test("validate core against validation") {
    validateSpec("meta/core", "meta/validation") { result =>
      assertEquals(result.results, Seq())
      assertEquals(result.count, 22)
      assertEquals(
        result.problems.ignoredKeywords,
        Set("$vocabulary", "$schema")
      )
    }
  }

  test("validate core against applicator") {
    validateSpec("meta/core", "meta/applicator") { result =>
      assertEquals(result.results, Seq())
      assertEquals(result.count, 84)
      assertEquals(
        result.problems.ignoredKeywords,
        Set("$vocabulary", "$schema")
      )
    }
  }

  test("validate validation against core") {
    validateSpec("meta/validation", "meta/core") { result =>
      assertEquals(result.results, Seq())
      assertEquals(result.count, 65)
      assertEquals(
        result.problems.ignoredKeywords,
        Set("$vocabulary", "$schema")
      )
    }
  }

  test("validate validation against validation") {
    validateSpec("meta/validation", "meta/validation") { result =>
      assertEquals(result.results, Seq())
      assertEquals(result.count, 22)
      assertEquals(
        result.problems.ignoredKeywords,
        Set("$vocabulary", "$schema")
      )
    }
  }

  test("validate validation against applicator") {
    validateSpec("meta/validation", "meta/applicator") { result =>
      assertEquals(result.results, Seq())
      assertEquals(result.count, 168)
      assertEquals(
        result.problems.ignoredKeywords,
        Set("$vocabulary", "$schema")
      )
    }
  }
}
