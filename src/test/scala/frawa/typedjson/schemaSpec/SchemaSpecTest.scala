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

class SchemaSpecTest extends FunSuite {
  implicit val zioParser = new ZioParser()

  def withSchemaValue(name: String)(f: Value => Unit)(implicit parser: Parser) {
    val text = Source.fromFile(s"./schemaSpec/${name}.json").getLines.mkString("\n")
    f(TestUtil.parseJsonValue(text))
  }

  def withSchemaSpec(name: String)(f: SchemaValue => Unit)(implicit parser: Parser) {
    withSchemaValue(name)(value => f(SchemaValue(value)))
  }

  def validateSpec(valueName: String, schemaName: String)(f: (Checked[ValidationResult], Set[String]) => Unit) {
    withSchemaSpec(schemaName) { schema =>
      withSchemaValue(valueName) { value =>
        val result = for {
          processor <- Processor(schema)(ValidationChecker())
          checked = processor.process(InnerValue(value))
        } yield {
          f(checked, processor.ignoredKeywords)
        }
        result.swap
          .map(message => fail("validating spec failed", clues(clue(message))))
          .swap
      }
    }
  }

  test("validate core against core") {
    validateSpec("core", "core") { (checked, ignored) =>
      assertEquals(checked.results, Seq())
      assertEquals(checked.count, 54)
      assertEquals(
        ignored,
        Set("$vocabulary", "$schema")
      )
    }
  }

  test("validate core against validation") {
    validateSpec("core", "validation") { (checked, ignored) =>
      assertEquals(checked.results, Seq())
      assertEquals(checked.count, 22)
      assertEquals(
        ignored,
        Set("$vocabulary", "$schema")
      )
    }
  }

  test("validate core against applicator") {
    validateSpec("core", "applicator") { (checked, ignored) =>
      assertEquals(checked.results, Seq())
      assertEquals(checked.count, 84)
      assertEquals(
        ignored,
        Set("$vocabulary", "$schema")
      )
    }
  }

  test("validate validation against core") {
    validateSpec("validation", "core") { (checked, ignored) =>
      assertEquals(checked.results, Seq())
      assertEquals(checked.count, 65)
      assertEquals(
        ignored,
        Set("$vocabulary", "$schema")
      )
    }
  }

  test("validate validation against validation") {
    validateSpec("validation", "validation") { (checked, ignored) =>
      assertEquals(checked.results, Seq())
      assertEquals(checked.count, 22)
      assertEquals(
        ignored,
        Set("$vocabulary", "$schema")
      )
    }
  }

  test("validate validation against applicator") {
    validateSpec("validation", "applicator") { (checked, ignored) =>
      assertEquals(checked.results, Seq())
      assertEquals(checked.count, 168)
      assertEquals(
        ignored,
        Set("$vocabulary", "$schema")
      )
    }
  }
}
