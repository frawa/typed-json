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

case class Processor[R] private[schema] (process: Processor.ProcessFun[R])

object Processor {
  type SchemaErrors = Checks.SchemaErrors

  type ProcessFun[R] = InnerValue => Checked[R]

  def apply[R](
      schema: SchemaValue
  )(checker: Checker[R]): Either[SchemaErrors, Processor[R]] = {
    implicit val resolver = LoadedSchemasResolver(schema)
    for {
      checks <- Checks.parseKeywords(schema)
      processor = Processor(process(checker)(checks))
    } yield (processor)
  }

  private def process[R](
      checker: Checker[R]
  )(checks: Checks): ProcessFun[R] = { value =>
    val checked = checks.checks.map(processOne(checker)(_)(value))
    merge(checked)
  }

  private def process2[R](checker: Checker[R], checks: Checks): ProcessFun[R] = { value =>
    val checked = checks.checks.map(processOne(checker)(_)(value))
    merge(checked)
  }
  private def noop[R]: ProcessFun[R]                                                 = value => Checked.valid[R]
  private def simpleCheck[R](checker: Checker[R], check: SimpleCheck): ProcessFun[R] = checker.check(check)
  private def seq[R](ps: Seq[ProcessFun[R]]): ProcessFun[R]                          = value => merge(ps.map(_.apply(value)))

  private def applyToArray[R](p: ProcessFun[R])(merge: Seq[Checked[R]] => ProcessFun[R]): ProcessFun[R] = {
    value: InnerValue =>
      value.value match {
        case ArrayValue(vs) => {
          val indexed = vs.zipWithIndex
            .map { case (v, index) =>
              InnerValue(v, value.pointer / index)
            }
          val checked = indexed.map(p)
          merge(checked)(value)
        }
        case _ => Checked.valid
      }
  }

  private def applyToObject[R](
      p: Map[String, ProcessFun[R]]
  )(merge: Seq[Checked[R]] => ProcessFun[R]): ProcessFun[R] = { value =>
    value.value match {
      case ObjectValue(vs) => {
        val inners = vs.view.map { case (key, v) =>
          (key, InnerValue(v, value.pointer / key))
        }.toMap
        val checked = inners.view.flatMap { case (key, v) => p.get(key).map(_.apply(v)) }.toSeq
        merge(checked)(value)
      }
      case _ => Checked.valid
    }
  }

  private def applyToValue[R](ps: Seq[ProcessFun[R]])(merge: Seq[Checked[R]] => ProcessFun[R]): ProcessFun[R] = {
    value =>
      val checked = ps.map(_.apply(value))
      merge(checked)(value)
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

  // private def checks[R](checker: Checker[R], checks: Checks): ProcessFun[R] = seq(
  //   checks.checks.map(check => simpleCheck(checker, check))
  // )

  // private def nestingCheck[R](checker: Checker[R], check: NestingCheck): ProcessFun[R] = checker.check(check)
  // private def fw[R](process: ProcessFun[R])(f)

  private def merge[R](checked: Seq[Checked[R]]): Checked[R] = {
    val valid           = checked.forall(_.valid)
    val results: Seq[R] = checked.flatMap(_.results)
    Checked(valid, results)
  }

  private def processOne[R](
      checker: Checker[R]
  )(check: Check): ProcessFun[R] =
    check match {
      case simple: SimpleCheck   => processSimple(checker)(simple)
      case nesting: NestingCheck => processNesting(checker)(nesting)
    }

  private def processSimple[R](checker: Checker[R])(check: SimpleCheck): ProcessFun[R] = checker.check(check)

  private def processNesting[R](
      checker: Checker[R]
  )(check: NestingCheck): ProcessFun[R] =
    check match {
      case c: ArrayItemsCheck       => checkArrayItems(checker, c)
      case c: ObjectPropertiesCheck => checkObjectProperties(checker, c)
      case c: NotCheck              => checkApplicator(checker, Seq(c.checks))(checker.nested(check))
      case c: AllOfCheck            => checkApplicator(checker, c.checks)(checker.nested(check))
      case c: AnyOfCheck            => checkApplicator(checker, c.checks)(checker.nested(check))
      case c: OneOfCheck            => checkApplicator(checker, c.checks)(checker.nested(check))
      case c: IfThenElseCheck       => checkIfThenElse(checker)(c)
    }

  private def checkArrayItems[R](checker: Checker[R], check: ArrayItemsCheck): ProcessFun[R] = {
    if (check.items.isDefined) {
      val p     = process(checker)(check.items.get)
      val merge = checker.nested(check)
      applyToArray(p)(merge)
    } else {
      noop
    }
  }

  private def checkObjectProperties[R](checker: Checker[R], check: ObjectPropertiesCheck): ProcessFun[R] = {
    if (check.properties.nonEmpty) {
      val p = check.properties.view
        .mapValues(process(checker)(_))
        .toMap
      val merge = checker.nested(check)
      applyToObject(p)(merge)
    } else {
      noop
    }
  }

  private def checkApplicator[R](checker: Checker[R], checks: Seq[Checks])(
      merge: Seq[Checked[R]] => ProcessFun[R]
  ): ProcessFun[R] = {
    val p = checks.map(process(checker)(_))
    applyToValue(p)(merge)
  }

  private def checkIfThenElse[R](checker: Checker[R])(check: IfThenElseCheck): ProcessFun[R] = { value =>
    check.ifChecks
      .map(process(checker)(_)(value))
      .flatMap { checked =>
        val branchChecks  = if (checked.valid) check.thenChecks else check.elseChecks
        val branchChecked = branchChecks.map(process(checker)(_)(value))
        branchChecked.map(checked => checker.nested(check)(Seq(checked))(value))
      }
      .getOrElse(Checked.valid)
  }

}
