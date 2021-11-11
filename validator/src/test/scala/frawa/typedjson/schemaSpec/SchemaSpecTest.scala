package frawa.typedjson.schemaSpec

import munit.FunSuite
import frawa.typedjson.schema.SchemaValue
import frawa.typedjson.parser.ZioParser
import frawa.typedjson.parser.Parser
import scala.io.Source
import frawa.typedjson.schema.TestUtil
import frawa.typedjson.schema.Processor
import frawa.typedjson.schema.ValidationChecker
import frawa.typedjson.schema.InnerValue
import frawa.typedjson.schema.Pointer
import frawa.typedjson.schema.LoadedSchemasResolver
import frawa.typedjson.parser.Value
import frawa.typedjson.schema.Checked
import frawa.typedjson.schema.ValidationResult
import frawa.typedjson.schema.SpecMetaSchemas
import TestUtil._
class SchemaSpecTest extends FunSuite {
  implicit val zioParser = new ZioParser()
  val resolver           = SpecMetaSchemas.lazyResolver
  val base               = SpecMetaSchemas.draft202012
  val lazyResolver       = Some(SpecMetaSchemas.lazyResolver)

  def withSchemaSpec(name: String)(f: SchemaValue => Unit) {
    val Some(schema) = resolver(base.resolve(name))
    f(schema)
  }

  def validateSpec(valueName: String, schemaName: String)(f: Checked[ValidationResult] => Unit) {
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
      assertEquals(checked.count, 46)
      assertEquals(
        checked.validation.ignoredKeywords,
        Set("$vocabulary", "$schema")
      )
    }
  }

  test("validate core against validation") {
    validateSpec("meta/core", "meta/validation") { checked =>
      assertEquals(checked.results, Seq())
      assertEquals(checked.count, 20)
      assertEquals(
        checked.validation.ignoredKeywords,
        Set("$vocabulary", "$schema")
      )
    }
  }

  test("validate core against applicator") {
    validateSpec("meta/core", "meta/applicator") { checked =>
      assertEquals(checked.results, Seq())
      assertEquals(checked.count, 58)
      assertEquals(
        checked.validation.ignoredKeywords,
        Set("$vocabulary", "$schema")
      )
    }
  }

  test("validate validation against core") {
    validateSpec("meta/validation", "meta/core") { checked =>
      assertEquals(checked.results, Seq())
      assertEquals(checked.count, 55)
      assertEquals(
        checked.validation.ignoredKeywords,
        Set("$vocabulary", "$schema")
      )
    }
  }

  test("validate validation against validation") {
    validateSpec("meta/validation", "meta/validation") { checked =>
      assertEquals(checked.results, Seq())
      assertEquals(checked.count, 20)
      assertEquals(
        checked.validation.ignoredKeywords,
        Set("$vocabulary", "$schema")
      )
    }
  }

  test("validate validation against applicator") {
    validateSpec("meta/validation", "meta/applicator") { checked =>
      assertEquals(checked.results, Seq())
      assertEquals(checked.count, 116)
      assertEquals(
        checked.validation.ignoredKeywords,
        Set("$vocabulary", "$schema")
      )
    }
  }
}
