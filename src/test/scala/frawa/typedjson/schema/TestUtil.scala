package frawa.typedjson.schema

import munit.FunSuite
import frawa.typedjson.parser.Value
import frawa.typedjson.parser.ZioParser
import frawa.typedjson.parser.Parser
import frawa.typedjson.parser.NumberValue
import frawa.typedjson.parser.BoolValue
import frawa.typedjson.parser.NullValue
import frawa.typedjson.parser.StringValue
import frawa.typedjson.parser.ArrayValue
import frawa.typedjson.parser.ObjectValue
import scala.reflect.internal.Reporter
import munit.Assertions._

object TestUtil {
  private def withParsedSchemaValue(text: String, parse: String => Either[String, SchemaValue])(
      f: SchemaValue => Unit
  ) {
    val withSchema = for {
      value <- parse(text)
    } yield {
      f(value)
    }
    withSchema.swap
      .map(message => fail("no schema", clues(clue(message))))
      .swap
  }

  def withSchema(text: String)(f: SchemaValue => Unit)(implicit parser: Parser) {
    withParsedSchemaValue(text, SchemaValue.apply)(f)
  }

}
