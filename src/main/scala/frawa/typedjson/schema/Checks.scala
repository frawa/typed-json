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

trait Checker[R] {
  def check(checks: Checks)(value: Value): R
}

case class SchemaError(message: String, pointer: Pointer = Pointer.empty) {
  def prefix(prefix: Pointer): SchemaError = SchemaError(message, prefix / pointer)
}

sealed trait Check
case class TrivialCheck(v: Boolean)                               extends Check
case object NullTypeCheck                                         extends Check
case object BooleanTypeCheck                                      extends Check
case object StringTypeCheck                                       extends Check
case object NumberTypeCheck                                       extends Check
case object ArrayTypeCheck                                        extends Check
case class ArrayItemsCheck(items: Option[Checks] = None)          extends Check
case object ObjectTypeCheck                                       extends Check
case class ObjectPropertiesCheck(properties: Map[String, Checks]) extends Check
case class ObjectRequiredCheck(names: Seq[String])                extends Check
case class NotCheck(checks: Checks)                               extends Check
case class AllOfCheck(checks: Seq[Checks])                        extends Check
case class AnyOfCheck(checks: Seq[Checks])                        extends Check
case class OneOfCheck(checks: Seq[Checks])                        extends Check
case class IfThenElseCheck(
    ifChecks: Option[Checks] = None,
    thenChecks: Option[Checks] = None,
    elseChecks: Option[Checks] = None
)                                                 extends Check
case class UnionTypeCheck(typeChecks: Seq[Check]) extends Check
case class EnumCheck(values: Seq[Value])          extends Check

case class Checks(
    schema: SchemaValue,
    checks: Seq[Check] = Seq.empty[Check],
    ignoredKeywords: Set[String] = Set.empty
) {
  import Util._

  private def withCheck(check: Check): Checks                         = this.copy(checks = checks :+ check)
  private def withChecks(checks: Checks)(f: Checks => Checks): Checks = f(checks).withIgnored(checks.ignoredKeywords)
  private def withChecks(checks: Map[String, Checks])(f: Map[String, Checks] => Check): Checks =
    withCheck(f(checks)).withIgnored(checks.values.flatMap(_.ignoredKeywords).toSet)
  private def withChecks(checks: Seq[Checks])(f: Seq[Checks] => Check): Checks =
    withCheck(f(checks)).withIgnored(checks.flatMap(_.ignoredKeywords).toSet)
  private def withChecks(
      values: Seq[Value]
  )(f: Seq[Checks] => Check)(implicit resolver: SchemaResolver): Either[SchemaErrors, Checks] = {
    val checks0 = values
      .map(v => Checks.parseKeywords(SchemaValue(v)))
      .toSeq
    for {
      checks <- sequenceAllLefts(checks0)
    } yield {
      withChecks(checks)(f)
    }
  }

  type SchemaErrors = Checks.SchemaErrors

  def withKeyword(keyword: String, value: Value)(implicit resolver: SchemaResolver): Either[SchemaErrors, Checks] =
    (keyword, value) match {
      // TODO validation vocabulary
      case ("type", StringValue(typeName)) =>
        Right(
          getTypeCheck(typeName)
            .map(withCheck(_))
            .getOrElse(withIgnored(keyword))
        )

      // TODO validation vocabulary
      case ("type", ArrayValue(values)) => {
        def typeNames = toStrings(values)
        Right(withCheck(UnionTypeCheck(typeNames.flatMap(getTypeCheck(_)))))
      }

      case ("not", value) =>
        for {
          checks <- Checks.parseKeywords(SchemaValue(value))
        } yield {
          withChecks(checks)(c => withCheck(NotCheck(c)))
        }

      case ("items", value) =>
        for {
          checks <- Checks.parseKeywords(SchemaValue(value))
        } yield {
          withChecks(checks) { checks =>
            updateCheck(ArrayItemsCheck())(check => check.copy(items = Some(checks)))
          }
        }

      case ("properties", ObjectValue(properties)) =>
        val propChecks = properties.view
          .mapValues(v => Checks.parseKeywords(SchemaValue(v)))
          .map {
            case (prop, Right(checks)) => Right((prop, checks))
            case (prop, Left(errors))  => Left(errors.map(_.prefix(Pointer.empty / prop)))
          }
          .toSeq
        for {
          propChecks1 <- sequenceFirstLeft(propChecks)
          checks = Map.from(propChecks1)
        } yield {
          withChecks(checks)(ObjectPropertiesCheck)
        }

      case ("required", ArrayValue(values)) => {
        def names = toStrings(values)
        Right(withCheck(ObjectRequiredCheck(names)))
      }

      case ("allOf", ArrayValue(values)) => {
        withChecks(values)(AllOfCheck)
      }

      case ("anyOf", ArrayValue(values)) => {
        withChecks(values)(AnyOfCheck)
      }

      case ("oneOf", ArrayValue(values)) => {
        withChecks(values)(OneOfCheck)
      }

      case ("if", value) =>
        updateChecks(SchemaValue(value))(IfThenElseCheck()) { (checks, check) =>
          check.copy(ifChecks = Some(checks))
        }

      case ("then", value) =>
        updateChecks(SchemaValue(value))(IfThenElseCheck()) { (checks, check) =>
          check.copy(thenChecks = Some(checks))
        }

      case ("else", value) =>
        updateChecks(SchemaValue(value))(IfThenElseCheck()) { (checks, check) =>
          check.copy(elseChecks = Some(checks))
        }

      // TODO validation vocabulary
      case ("enum", ArrayValue(values)) => {
        Right(withCheck(EnumCheck(values)))
      }

      // TODO validation vocabulary
      case ("const", value) => {
        Right(withCheck(EnumCheck(Seq(value))))
      }

      case ("$id", StringValue(_)) => {
        // handled during load
        Right(this)
      }

      case ("$anchor", StringValue(_)) => {
        // handled during load
        Right(this)
      }

      case ("$defs", ObjectValue(_)) => {
        // handled during load
        Right(this)
      }

      case ("$ref", StringValue(ref)) => {
        for {
          schema <- resolver
            .resolveRef(ref)
            .map(Right(_))
            .getOrElse(Left(Seq(SchemaError(s"""missing reference "${ref}""""))))
          checks <- Checks.parseKeywords(schema)
        } yield {
          withChecks(checks)(identity)
        }
      }

      case _ => Right(withIgnored(keyword))
    }

  private def updateCheck[C <: Check: ClassTag](newCheck: => C)(f: C => C): Checks = {
    val checks1: Seq[Check] =
      if (
        checks.exists {
          case check: C => true
          case other    => false
        }
      ) {
        checks
      } else {
        checks :+ newCheck
      }
    this.copy(checks = checks1.map {
      case check: C => f(check)
      case other    => other
    })
  }

  private def updateChecks[C <: Check: ClassTag](
      schema: SchemaValue
  )(newCheck: => C)(f: (Checks, C) => C)(implicit resolver: SchemaResolver): Either[SchemaErrors, Checks] = {
    for {
      checks <- Checks.parseKeywords(schema)
    } yield {
      withChecks(checks) { checks =>
        updateCheck(newCheck)(f(checks, _))
      }
    }
  }

  private def withIgnored(keyword: String): Checks =
    this.copy(ignoredKeywords = ignoredKeywords + keyword)

  private def withIgnored(ignored: Set[String]): Checks =
    this.copy(ignoredKeywords = ignoredKeywords.concat(ignored))

  private def getTypeCheck(typeName: String): Option[Check] =
    typeName match {
      case "null"    => Some(NullTypeCheck)
      case "boolean" => Some(BooleanTypeCheck)
      case "string"  => Some(StringTypeCheck)
      case "number"  => Some(NumberTypeCheck)
      case "array"   => Some(ArrayTypeCheck)
      case "object"  => Some(ObjectTypeCheck)
      case _         => None
    }

  def processor[R](checker: Checker[R]): Processor[R] =
    Processor(value => checker.check(this)(value))

}

object Checks {
  type SchemaErrors = Seq[SchemaError]

  def parseKeywords(schema: SchemaValue)(implicit resolver: SchemaResolver): Either[SchemaErrors, Checks] =
    schema.value match {
      case BoolValue(v) => Right(Checks(schema).withCheck(TrivialCheck(v)))
      case ObjectValue(keywords) =>
        if (keywords.isEmpty) {
          Right(Checks(schema).withCheck(TrivialCheck(true)))
        } else {
          keywords
            .foldLeft[Either[SchemaErrors, Checks]](Right(Checks(schema))) { case (checks, (keyword, value)) =>
              val prefix = Pointer.empty / keyword
              checks
                .flatMap(_.withKeyword(keyword, value))
                .swap
                .map(_.map(_.prefix(prefix)))
                .swap
            }
        }
      case _ => Left(Seq(SchemaError(s"invalid schema ${schema}")))
    }
}