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

class SchemaSpecTest extends FunSuite {
  implicit val zioParser = new ZioParser();

  def withSchemaValue(name: String)(f: Value => Unit)(implicit parser: Parser) {
    val text = Source.fromFile(s"./schemaSpec/${name}.json").getLines.mkString("\n")
    f(TestUtil.parseValue(text))
  }

  def withSchemaSpec(name: String)(f: SchemaValue => Unit)(implicit parser: Parser) {
    withSchemaValue(name)(value => f(SchemaValue(value)))
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

  test("validation on core".only) {
    withSchemaSpec("validation") { schema =>
      withSchemaValue("core") { value =>
        val result = for {
          processor <- Processor(schema)(ValidationChecker())
          checked = processor.process(InnerValue(value))
        } yield {
          checked
        }
        assertEquals(result.map(_.count).getOrElse(-1), 16)
      }
    }
  }

}
