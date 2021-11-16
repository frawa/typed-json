package frawa.typedjson.schema

import frawa.typedjson.parser.Value
import frawa.typedjson.parser.Parser
import munit.Assertions._

object TestUtil {
  implicit val lazyResolver: Option[LoadedSchemasResolver.LazyResolver] = None

  def parseJsonValue(text: String)(implicit parser: Parser): Value = {
    parser
      .parse(text)
      .swap
      .map(message => fail("no json value", clues(clue(message))))
      .swap
      .toOption
      .get
  }

  def withSchema(text: String)(f: SchemaValue => Unit)(implicit parser: Parser): Unit = {
    f(SchemaValue(parseJsonValue(text)))
  }

  def withLoadedSchemas(texts: Seq[String])(f: LoadedSchemasResolver => Unit)(implicit parser: Parser): Unit = {
    val schemas  = texts.map(t => parseJsonValue(t)).map(SchemaValue(_))
    val resolver = LoadedSchemasResolver(schemas)
    f(resolver)
  }

  def assertChecked[R](
      checker: Checker[R]
  )(schema: SchemaValue, valueText: String)(
      f: Checked[R] => Unit
  )(implicit parser: Parser, lazyResolver: Option[LoadedSchemasResolver.LazyResolver]): Either[Nothing, Unit] = {
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
  ): Either[Nothing, Unit] = {
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
  )(schema: SchemaValue)(
      f: Processor[R] => Unit
  )(implicit lazyResolver: Option[LoadedSchemasResolver.LazyResolver]): Either[Nothing, Unit] = {
    withProcessor(checker)(schema, lazyResolver) { processor =>
      assertEquals(processor.validation.ignoredKeywords, Set.empty[String], "new keywords")
      f(processor)
    }
  }

}
