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
  implicit val zioParser = new ZioParser();

  def withSchemaValue(name: String)(f: Value => Unit)(implicit parser: Parser) {
    val text = Source.fromFile(s"./schemaSpec/${name}.json").getLines.mkString("\n")
    f(TestUtil.parseValue(text))
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
      }
    }
  }

  test("core") {
    withSchemaSpec("core") { schema =>
      val result = for {
        processor <- Processor(schema)(ValidationChecker())
        checked = processor.process(InnerValue(schema.value))
      } yield {
        checked
      }
    }
  }

  test("validate core against validation") {
    validateSpec("core", "validation") { (checked, ignored) =>
      assertEquals(checked.count, 16)
      assertEquals(
        ignored,
        Set(
          "format",
          "$dynamicAnchor",
          "additionalProperties",
          "exclusiveMinimum",
          "$vocabulary",
          "default",
          "minItems",
          "title",
          "minimum",
          "type",
          "$schema",
          "uniqueItems"
        )
      )
    }
  }

  test("validate core against applicator") {
    validateSpec("core", "applicator") { (checked, ignored) =>
      assertEquals(checked.count, 7)
      assertEquals(
        ignored,
        Set(
          "$dynamicAnchor",
          "$vocabulary",
          "additionalProperties",
          "title",
          "propertyNames",
          "default",
          "minItems",
          "$schema",
          "$dynamicRef"
        )
      )
    }
  }
}
