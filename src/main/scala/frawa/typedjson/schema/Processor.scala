package frawa.typedjson.schema

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
import java.net.URI
import scala.reflect.ClassTag

case class SchemaValue(value: Value)

object SchemaValue {
  def apply(json: String)(implicit parser: Parser): Either[String, SchemaValue] =
    for {
      value <- parser.parse(json)
    } yield SchemaValue(value)
}

// case object RootSchemaResolver extends SchemaResolver {
//   override def resolve(uri: URI): Option[SchemaValue] = None
// }

// case class RelativeSchemaResolver(id: String, resolver: SchemaResolver) extends SchemaResolver {
//   override val base                                   = Some(URI.create(id).normalize())
//   override def resolve(uri: URI): Option[SchemaValue] = resolver.resolve(uri)
// }

case class Processor[R] private[schema] (process: InnerValue => R)

object Processor {
  type SchemaErrors = Checks.SchemaErrors

  def apply[R](
      schema: SchemaValue
  )(checker: Checker[R]): Either[SchemaErrors, Processor[R]] = {
    implicit val resolver = LoadedSchemasResolver(schema)
    for {
      checks <- Checks.parseKeywords(schema)
    } yield (processor(checker)(checks))
  }

  def processor[R](checker: Checker[R])(checks: Checks): Processor[R] =
    Processor(value => checker.check(checks)(value))

  def processIndexed[R](
      checker: Checker[R]
  )(checks: Checks)(values: Seq[Value], pointer: Pointer): Seq[R] = {
    val processor = Processor.processor(checker)(checks)
    values.zipWithIndex
      .map { case (value, index) =>
        processor.process(InnerValue(value, pointer / index))
      }
  }

  def processMap[R](
      checker: Checker[R]
  )(checks: Map[String, Checks])(values: Map[String, Value], pointer: Pointer)(f: (Option[R], String) => R): Seq[R] = {
    values.map { case (key, value) =>
      f(
        checks
          .get(key)
          .map(Processor.processor(checker)(_))
          .map(_.process(InnerValue(value, pointer / key))),
        key
      )
    }.toSeq
  }

}
