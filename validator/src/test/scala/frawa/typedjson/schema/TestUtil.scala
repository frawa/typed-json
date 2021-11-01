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

  def parseJsonValue(text: String)(implicit parser: Parser): Value = {
    parser
      .parse(text)
      .swap
      .map(message => fail("no json value", clues(clue(message))))
      .swap
      .toOption
      .get
  }

  def withSchema(text: String)(f: SchemaValue => Unit)(implicit parser: Parser) = {
    f(SchemaValue(parseJsonValue(text)))
  }

  def withLoadedSchemas(texts: Seq[String])(f: LoadedSchemasResolver => Unit)(implicit parser: Parser) = {
    val schemas  = texts.map(t => parseJsonValue(t)).map(SchemaValue(_))
    val resolver = LoadedSchemasResolver(schemas)
    f(resolver)
  }

  def assertChecked[R](
      checker: Checker[R]
  )(schema: SchemaValue, valueText: String)(f: Checked[R] => Unit)(implicit parser: Parser) = {
    withStrictProcessor(checker)(schema) { processor =>
      val value   = parseJsonValue(valueText)
      val checked = processor(InnerValue(value))
      f(checked)
    }
  }

  def withProcessor[R](
      checker: Checker[R]
  )(schema: SchemaValue, lazyResolver: Option[LoadedSchemasResolver.LazyResolver] = None)(
      f: Processor[R] => Unit
  )(implicit parser: Parser) = {
    val result = for {
      processor <- Processor(schema, lazyResolver)(checker)
    } yield {
      f(processor)
    }
    result.swap
      .map(message => fail("creating processor failed", clues(clue(message))))
      .swap
  }

  def withStrictProcessor[R](
      checker: Checker[R]
  )(schema: SchemaValue, lazyResolver: Option[LoadedSchemasResolver.LazyResolver] = None)(
      f: Processor[R] => Unit
  )(implicit parser: Parser) = {
    withProcessor(checker)(schema, lazyResolver) { processor =>
      assertEquals(processor.validation.ignoredKeywords, Set.empty[String], "new keywords")
      f(processor)
    }
  }

}
