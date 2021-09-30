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
import scala.util.matching.Regex

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

case class Processor[R] private[schema] (process: Processor.ProcessFun[R], ignoredKeywords: Set[String])

object Processor {
  type SchemaErrors = Checks.SchemaErrors

  type ProcessFun[R] = InnerValue => Checked[R]
  type MergeFun[R]   = Seq[Checked[R]] => ProcessFun[R]

  def apply[R](schema: SchemaValue)(checker: Checker[R]): Either[SchemaErrors, Processor[R]] = {
    implicit val resolver = LoadedSchemasResolver(schema)
    for {
      checks <- Checks.parseKeywords(schema)
      processor = Processor(all(checker, checks), checks.ignoredKeywords)
    } yield (processor)
  }

  private def all[R](checker: Checker[R], checks: Checks): ProcessFun[R] = {
    seq(checks.checks.map(one(checker, _)))
  }

  private def noop[R]: ProcessFun[R]                                            = _ => Checked.valid[R]
  private def simple[R](checker: Checker[R], check: SimpleCheck): ProcessFun[R] = checker.check(check)
  private def seq[R](ps: Seq[ProcessFun[R]]): ProcessFun[R]                     = value => Checked.merge(ps.map(_.apply(value)))
  private def option[R](p: Option[ProcessFun[R]]): ProcessFun[R] = {
    p.getOrElse(noop)
  }

  private def applyToArray[R](pPrefix: Seq[ProcessFun[R]], pItems: ProcessFun[R])(merge: MergeFun[R]): ProcessFun[R] = {
    value: InnerValue =>
      value.value match {
        case ArrayValue(vs) => {
          val indexed = vs.zipWithIndex
            .map { case (v, index) =>
              InnerValue(v, value.pointer / index)
            }
          val checkedPrefix = pPrefix.zip(indexed).map { case (p, v) => p(v) }
          val checked       = indexed.drop(pPrefix.length).map(pItems)
          merge(checkedPrefix ++ checked)(value)
        }
        case _ => Checked.valid
      }
  }

  private def applyToObject[R](
      ps: => Seq[(String => Boolean, () => ProcessFun[R])]
  )(merge: MergeFun[R]): ProcessFun[R] = { value =>
    value.value match {
      case ObjectValue(vs) => {
        val checked = vs.view
          .map { case (key, v) =>
            (key, InnerValue(v, value.pointer / key))
          }
          .flatMap { case (key, inner) =>
            val p = ps.find(_._1(key)).map(_._2)
            p.map(p => p().apply(inner))
          }
          .toSeq
        merge(checked)(value)
      }
      case _ => Checked.valid
    }
  }

  private def applyToValue[R](ps: Seq[ProcessFun[R]])(merge: MergeFun[R]): ProcessFun[R] = { value =>
    val checked = ps.map(_.apply(value))
    merge(checked)(value)
  }

  private def applyCondition[R](pIf: ProcessFun[R], pThen: ProcessFun[R], pElse: ProcessFun[R])(
      merge: MergeFun[R]
  ): ProcessFun[R] = { value =>
    val ifChecked = pIf(value)
    val pBranch   = if (ifChecked.valid) pThen else pElse
    val checked   = pBranch(value)
    merge(Seq(checked))(value)
  }

  private def one[R](checker: Checker[R], check: Check): ProcessFun[R] =
    check match {
      case c: SimpleCheck  => simple(checker, c)
      case c: NestingCheck => nesting(checker, c)
    }

  private def nesting[R](checker: Checker[R], check: NestingCheck): ProcessFun[R] =
    check match {
      case c: ArrayItemsCheck       => checkArrayItems(checker, c)
      case c: ObjectPropertiesCheck => checkObjectProperties(checker, c)
      case c: NotCheck              => checkApplicator(checker, Seq(c.checks))(checker.nested(check))
      case c: AllOfCheck            => checkApplicator(checker, c.checks)(checker.nested(check))
      case c: AnyOfCheck            => checkApplicator(checker, c.checks)(checker.nested(check))
      case c: OneOfCheck            => checkApplicator(checker, c.checks)(checker.nested(check))
      case c: IfThenElseCheck       => checkIfThenElse(checker, c)
      case c: PropertyNamesCheck    => checkPropertyNames(checker, c)
      case c: DynamicRefCheck       => checkDynamicRef(checker, c)
      case c: DependentSchemasCheck => checkDependentSchemas(checker, c)
      case c: ContainsCheck         => checkContains(checker, c)
    }

  private def checkArrayItems[R](checker: Checker[R], check: ArrayItemsCheck): ProcessFun[R] = {
    val pPrefix = check.prefixItems.map(all(checker, _))
    val pItems  = option(check.items.map(all(checker, _)))
    val merge   = checker.nested(check)
    applyToArray(pPrefix, pItems)(merge)
  }

  private def checkObjectProperties[R](checker: Checker[R], check: ObjectPropertiesCheck): ProcessFun[R] = {
    val ps1 = check.properties.map { case (key, checks) =>
      ({ k: String => k == key }, () => all(checker, checks))
    }.toSeq
    val ps2 = check.patternProperties.map { case (regex, checks) =>
      val r = regex.r
      (
        { k: String => r.findFirstIn(k).isDefined },
        () => all(checker, checks)
      )
    }.toSeq
    val ps3 = check.additionalProperties.map { checks => ({ k: String => true }, () => all(checker, checks)) }.toSeq

    val ps = ps1 ++ ps2 ++ ps3
    if (ps.nonEmpty) {
      val merge = checker.nested(check)
      applyToObject(ps)(merge)
    } else {
      noop
    }
  }

  private def checkApplicator[R](checker: Checker[R], checks: Seq[Checks])(merge: MergeFun[R]): ProcessFun[R] = {
    val p = checks.map(all(checker, (_)))
    applyToValue(p)(merge)
  }

  private def checkIfThenElse[R](checker: Checker[R], check: IfThenElseCheck): ProcessFun[R] = {
    check.ifChecks
      .map(ifChecks =>
        applyCondition(
          all(checker, ifChecks),
          option(check.thenChecks.map(all(checker, _))),
          option(check.elseChecks.map(all(checker, _)))
        )(checker.nested(check))
      )
      .getOrElse(noop)
  }

  private def checkPropertyNames[R](checker: Checker[R], check: PropertyNamesCheck): ProcessFun[R] = { value =>
    value.value match {
      case ObjectValue(vs) => {
        val p     = all(checker, check.checks)
        val merge = checker.nested(check)
        val names = vs.keySet
        val checked = names.map { name =>
          p(InnerValue(StringValue(name), value.pointer / name))
        }.toSeq
        merge(checked)(value)
      }
      case _ => Checked.valid
    }
  }

  private def checkDynamicRef[R](checker: Checker[R], check: DynamicRefCheck): ProcessFun[R] = {
    check
      .resolve()
      .map(all(checker, _))
      .swap
      .map { e =>
        println("FW", e)
        e
      }
      .swap
      .getOrElse(noop)
  }

  private def checkDependentSchemas[R](checker: Checker[R], check: DependentSchemasCheck): ProcessFun[R] = { value =>
    value.value match {
      case ObjectValue(v) =>
        val ps      = v.keySet.flatMap(check.checks.get(_)).map(all(checker, _)).toSeq
        val merge   = checker.nested(check)
        val checked = ps.map(_.apply(value))
        merge(checked)(value)
      case _ => Checked.valid
    }
  }

  private def checkContains[R](checker: Checker[R], check: ContainsCheck): ProcessFun[R] = { value =>
    value.value match {
      case ArrayValue(vs) =>
        val p     = option(check.schema.map(all(checker, _)))
        val merge = checker.nested(check)
        val indexed = vs.zipWithIndex
          .map { case (v, index) =>
            InnerValue(v, value.pointer / index)
          }
        val checked = indexed.map(p(_))
        merge(checked)(value)
      case _ => Checked.valid
    }
  }
}
