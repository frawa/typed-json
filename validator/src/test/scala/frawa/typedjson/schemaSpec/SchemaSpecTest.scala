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

package frawa.typedjson.schemaSpec

import munit.FunSuite
import frawa.typedjson.schema.SchemaValue
import frawa.typedjson.parser.ZioParser
import frawa.typedjson.schema.ValidationChecker
import frawa.typedjson.schema.Checked
import frawa.typedjson.schema.ValidationResult
import frawa.typedjson.schema.SpecMetaSchemas
import frawa.typedjson.schema.LoadedSchemasResolver
import frawa.typedjson.schema.InnerValue

import frawa.typedjson.schema.TestUtil._

class SchemaSpecTest extends FunSuite {
  implicit val zioParser: ZioParser = new ZioParser()

  val resolver                                               = SpecMetaSchemas.lazyResolver
  val base                                                   = SpecMetaSchemas.draft202012
  val lazyResolver: Some[LoadedSchemasResolver.LazyResolver] = Some(SpecMetaSchemas.lazyResolver)

  def withSchemaSpec(name: String)(f: SchemaValue => Unit): Unit = {
    val Some(schema) = resolver(base.resolve(name))
    f(schema)
  }

  def validateSpec(valueName: String, schemaName: String)(f: Checked[ValidationResult] => Unit): Unit = {
    withSchemaSpec(schemaName) { schema =>
      withSchemaSpec(valueName) { value =>
        withProcessor(ValidationChecker())(schema, lazyResolver) { processor =>
          val checked = processor(InnerValue(value.value))
          f(checked)
        }
      }
    }
  }

  test("validate core against core") {
    validateSpec("meta/core", "meta/core") { checked =>
      assertEquals(checked.results, Seq())
      assertEquals(checked.count, 54)
      assertEquals(
        checked.validation.ignoredKeywords,
        Set("$vocabulary", "$schema")
      )
    }
  }

  test("validate core against validation") {
    validateSpec("meta/core", "meta/validation") { checked =>
      assertEquals(checked.results, Seq())
      assertEquals(checked.count, 22)
      assertEquals(
        checked.validation.ignoredKeywords,
        Set("$vocabulary", "$schema")
      )
    }
  }

  test("validate core against applicator") {
    validateSpec("meta/core", "meta/applicator") { checked =>
      assertEquals(checked.results, Seq())
      assertEquals(checked.count, 84)
      assertEquals(
        checked.validation.ignoredKeywords,
        Set("$vocabulary", "$schema")
      )
    }
  }

  test("validate validation against core") {
    validateSpec("meta/validation", "meta/core") { checked =>
      assertEquals(checked.results, Seq())
      assertEquals(checked.count, 65)
      assertEquals(
        checked.validation.ignoredKeywords,
        Set("$vocabulary", "$schema")
      )
    }
  }

  test("validate validation against validation") {
    validateSpec("meta/validation", "meta/validation") { checked =>
      assertEquals(checked.results, Seq())
      assertEquals(checked.count, 22)
      assertEquals(
        checked.validation.ignoredKeywords,
        Set("$vocabulary", "$schema")
      )
    }
  }

  test("validate validation against applicator") {
    validateSpec("meta/validation", "meta/applicator") { checked =>
      assertEquals(checked.results, Seq())
      assertEquals(checked.count, 168)
      assertEquals(
        checked.validation.ignoredKeywords,
        Set("$vocabulary", "$schema")
      )
    }
  }
}
