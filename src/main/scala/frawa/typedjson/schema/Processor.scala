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

case class Processor[R] private[schema] (process: InnerValue => Checked[R])

object Processor {
  type SchemaErrors = Checks.SchemaErrors

  type ProcessFun[R] = InnerValue => Checked[R]

  def apply[R](
      schema: SchemaValue
  )(checker: Checker[R]): Either[SchemaErrors, Processor[R]] = {
    implicit val resolver = LoadedSchemasResolver(schema)
    for {
      checks <- Checks.parseKeywords(schema)
    } yield (processor(checker)(checks))
  }

  private def processor[R](checker: Checker[R])(checks: Checks): Processor[R] =
    Processor(value => process(checker)(checks)(value))

  private def process[R](
      checker: Checker[R]
  )(checks: Checks): ProcessFun[R] = { value =>
    val checked = checks.checks.map(processOne(checker)(_)(value))
    merge(checked)
  }

  private def merge[R](checked: Seq[Checked[R]]): Checked[R] = {
    val valid           = checked.forall(_.valid)
    val results: Seq[R] = checked.flatMap(_.results)
    Checked(valid, results)
  }

  private def processOne[R](
      checker: Checker[R]
  )(check: Check): ProcessFun[R] = value =>
    check match {
      case simple: SimpleCheck   => checker.check(simple)(value)
      case nesting: NestingCheck => processNesting(checker)(nesting)(value)
    }

  private def processNesting[R](
      checker: Checker[R]
  )(check: NestingCheck): ProcessFun[R] =
    check match {
      case c: ArrayItemsCheck       => checkArrayItems(checker)(c)
      case c: ObjectPropertiesCheck => checkObjectProperties(checker)(c)
      case c: NotCheck              => checkNot(checker)(c)
      case c: AllOfCheck            => checkApplicator(checker)(checker.nested(check))(c.checks)
      case c: AnyOfCheck            => checkApplicator(checker)(checker.nested(check))(c.checks)
      case c: OneOfCheck            => checkApplicator(checker)(checker.nested(check))(c.checks)
      case c @ IfThenElseCheck(ifChecks, thenChecks, elseChecks) =>
        checkIfThenElse(checker)(c)(ifChecks, thenChecks, elseChecks)
    }

  private def checkArrayItems[R](checker: Checker[R])(check: ArrayItemsCheck): ProcessFun[R] = value =>
    value.value match {
      case ArrayValue(itemValues) => {
        if (check.items.isDefined && itemValues.nonEmpty) {
          val results = processIndexed(process(checker)(check.items.get))(itemValues, value.pointer)
          checker.nested(check)(results)(value)
        } else {
          Checked.valid
        }
      }
      case _ => Checked.valid
    }

  private def checkObjectProperties[R](checker: Checker[R])(check: ObjectPropertiesCheck): ProcessFun[R] = value =>
    value.value match {
      case ObjectValue(propertiesValues) => {
        val results = processMap(process(checker))(check.properties)(propertiesValues, value.pointer)
        if (propertiesValues.isEmpty) {
          Checked.valid
        } else {
          checker.nested(check)(results)(value)
        }
      }
      case _ => Checked.valid
    }

  private def checkNot[R](checker: Checker[R])(check: NotCheck): ProcessFun[R] = { value =>
    val checked = processor(checker)(check.checks).process(value)
    checker.nested(check)(Seq(checked))(value)
  }

  private def checkApplicator[R](
      checker: Checker[R]
  )(
      f: Seq[Checked[R]] => ProcessFun[R]
  )(checks: Seq[Checks]): ProcessFun[R] = { value =>
    val checked = checks.map(processor(checker)(_).process(value))
    f(checked)(value)
  }

  private def processIndexed[R](process: ProcessFun[R])(values: Seq[Value], pointer: Pointer): Seq[Checked[R]] = {
    values.zipWithIndex
      .map { case (value, index) =>
        process(InnerValue(value, pointer / index))
      }
  }

  private def checkIfThenElse[R](
      checker: Checker[R]
  )(
      check: IfThenElseCheck
  )(ifChecks: Option[Checks], thenChecks: Option[Checks], elseChecks: Option[Checks]): ProcessFun[R] = { value =>
    ifChecks
      .map(processor(checker)(_).process(value))
      .flatMap { checked =>
        val branchChecks  = if (checked.valid) thenChecks else elseChecks
        val branchChecked = branchChecks.map(processor(checker)(_).process(value))
        branchChecked.map(checked => checker.nested(check)(Seq(checked))(value))
      }
      .getOrElse(Checked.valid)
  }

  private def processMap[R](
      process: Checks => ProcessFun[R]
  )(checks: Map[String, Checks])(values: Map[String, Value], pointer: Pointer): Seq[Checked[R]] = {
    values.map { case (key, value) =>
      checks
        .get(key)
        .map(process(_)(InnerValue(value, pointer / key)))
        .getOrElse(Checked(true, Seq()))
    }.toSeq
  }
}
