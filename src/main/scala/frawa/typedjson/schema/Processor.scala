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
  )(checker: Checker[R]): Either[SchemaErrors, Processor[Checked[R]]] = {
    implicit val resolver = LoadedSchemasResolver(schema)
    for {
      checks <- Checks.parseKeywords(schema)
    } yield (processor(checker)(checks))
  }

  def processor[R](checker: Checker[R])(checks: Checks): Processor[Checked[R]] =
    Processor(value => process(checker)(checks)(value))

  def process[R](
      checker: Checker[R]
  )(checks: Checks)(value: InnerValue): Checked[R] = {
    val checked = checks.checks.map(processOne(checker)(_)(value))
    merge(checked)
  }

  private def merge[R](checked: Seq[Checked[R]]): Checked[R] = {
    val valid           = checked.forall(_.valid)
    val results: Seq[R] = checked.flatMap(_.results)
    Checked(valid, results)
  }

  // private def anyOf[R](checked: Seq[Checked[R]]): Checked[R] = {
  //   val valid           = checked.exists(_.valid)
  //   val results: Seq[R] = checked.flatMap(_.results)
  //   Checked(valid, results)
  // }

  // private def oneOf[R](checked: Seq[Checked[R]]): Checked[R] = {
  //   val valid           = checked.count(_.valid) == 1
  //   val results: Seq[R] = checked.flatMap(_.results)
  //   Checked(valid, results)
  // }

  // private def not[R](checked: Checked[R]): Checked[R] = {
  //   Checked(!checked.valid, Seq())
  // }

  private def processOne[R](
      checker: Checker[R]
  )(check: Check)(value: InnerValue): Checked[R] =
    check match {
      case simple: SimpleCheck   => checker.check(simple)(value)
      case nesting: NestingCheck => processNesting(checker)(nesting)(value)
    }

  private def processNesting[R](
      checker: Checker[R]
  )(check: NestingCheck)(value: InnerValue): Checked[R] =
    check match {
      case ArrayItemsCheck(items)            => checkArrayItems(checker)(items, value)
      case ObjectPropertiesCheck(properties) => checkObjectProperties(checker)(properties, value)
      case NotCheck(checks)                  => checkNot(checker)(checks, value)
      case AllOfCheck(checks)                => checkApplicator(checker)(checker.applicate(AllOfKind))(checks, value)
      case AnyOfCheck(checks)                => checkApplicator(checker)(checker.applicate(AnyOfKind))(checks, value)
      case OneOfCheck(checks)                => checkApplicator(checker)(checker.applicate(OneOfKind))(checks, value)
      case IfThenElseCheck(ifChecks, thenChecks, elseChecks) =>
        checkIfThenElse(checker)(ifChecks, thenChecks, elseChecks, value)
    }

  private def checkArrayItems[R](
      checker: Checker[R]
  )(items: Option[Checks], value: InnerValue): Checked[R] =
    value.value match {
      case ArrayValue(itemValues) => {
        if (items.isDefined && itemValues.nonEmpty) {
          merge(
            processIndexed(checker)(items.get)(itemValues, value.pointer)
          )
        } else {
          Checked(true, Seq())
        }
      }
      case _ => Checked(true, Seq())
    }

  def checkObjectProperties[R](
      checker: Checker[R]
  )(properties: Map[String, Checks], value: InnerValue): Checked[R] =
    value.value match {
      case ObjectValue(propertiesValues) => {
        val results = processMap(checker)(properties)(propertiesValues, value.pointer)
        if (results.isEmpty) {
          Checked(true, Seq())
        } else {
          merge(results)
        }
      }
      case _ => Checked(true, Seq())
    }

  private def checkNot[R](
      checker: Checker[R]
  )(checks: Checks, value: InnerValue): Checked[R] = {
    val checked = processor(checker)(checks).process(value)
    checker.applicate(NotKind)(Seq(checked))(value)
  }

  private def checkApplicator[R](
      checker: Checker[R]
  )(
      f: Seq[Checked[R]] => InnerValue => Checked[R]
  )(checks: Seq[Checks], value: InnerValue): Checked[R] = {
    val checked = checks.map(processor(checker)(_).process(value))
    f(checked)(value)
  }

  private def processIndexed[R](
      checker: Checker[R]
  )(checks: Checks)(values: Seq[Value], pointer: Pointer): Seq[Checked[R]] = {
    val processor1 = processor(checker)(checks)
    values.zipWithIndex
      .map { case (value, index) =>
        processor1.process(InnerValue(value, pointer / index))
      }
  }

  private def checkIfThenElse[R](
      checker: Checker[R]
  )(
      ifChecks: Option[Checks],
      thenChecks: Option[Checks],
      elseChecks: Option[Checks],
      value: InnerValue
  ): Checked[R] = {
    ifChecks
      .map(processor(checker)(_).process(value))
      .flatMap { checked =>
        if (checked.valid) {
          thenChecks.map(processor(checker)(_).process(value))
        } else {
          elseChecks.map(processor(checker)(_).process(value))
        }
      }
      .getOrElse(Checked(true, Seq()))
  }

  private def processMap[R](
      checker: Checker[R]
  )(
      checks: Map[String, Checks]
  )(values: Map[String, Value], pointer: Pointer): Seq[Checked[R]] = {
    values.map { case (key, value) =>
      checks
        .get(key)
        .map(Processor.processor(checker)(_))
        .map(_.process(InnerValue(value, pointer / key)))
        .getOrElse(Checked(true, Seq()))
    }.toSeq
  }
}
