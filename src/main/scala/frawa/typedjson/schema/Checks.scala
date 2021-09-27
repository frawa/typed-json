package frawa.typedjson.schema

import frawa.typedjson.parser.ArrayValue
import frawa.typedjson.parser.BoolValue
import frawa.typedjson.parser.NullValue
import frawa.typedjson.parser.NumberValue
import frawa.typedjson.parser.ObjectValue
import frawa.typedjson.parser.Parser
import frawa.typedjson.parser.StringValue
import frawa.typedjson.parser.Value
import frawa.typedjson.parser.ZioParser

import java.net.URI
import scala.reflect.ClassTag
import scala.reflect.internal.Reporter

case class InnerValue(value: Value, pointer: Pointer = Pointer.empty)

case class SchemaError(message: String, pointer: Pointer = Pointer.empty) {
  def prefix(prefix: Pointer): SchemaError = SchemaError(message, prefix / pointer)
}

sealed trait Check
sealed trait SimpleCheck  extends Check
sealed trait TypeCheck    extends SimpleCheck
sealed trait NestingCheck extends Check

case class TrivialCheck(v: Boolean)                      extends SimpleCheck
case object NullTypeCheck                                extends TypeCheck
case object BooleanTypeCheck                             extends TypeCheck
case object StringTypeCheck                              extends TypeCheck
case object NumberTypeCheck                              extends TypeCheck
case object IntegerTypeCheck                             extends TypeCheck
case object ArrayTypeCheck                               extends TypeCheck
case object ObjectTypeCheck                              extends TypeCheck
case class ObjectRequiredCheck(names: Seq[String])       extends SimpleCheck
case class NotCheck(checks: Checks)                      extends NestingCheck
case class AllOfCheck(checks: Seq[Checks])               extends NestingCheck
case class AnyOfCheck(checks: Seq[Checks])               extends NestingCheck
case class OneOfCheck(checks: Seq[Checks])               extends NestingCheck
case class UnionTypeCheck(typeChecks: Seq[TypeCheck])    extends SimpleCheck
case class EnumCheck(values: Seq[Value])                 extends SimpleCheck
case class ArrayItemsCheck(items: Option[Checks] = None) extends NestingCheck
case class ObjectPropertiesCheck(
    properties: Map[String, Checks] = Map(),
    patternProperties: Map[String, Checks] = Map(),
    additionalProperties: Option[Checks] = None
) extends NestingCheck
case class IfThenElseCheck(
    ifChecks: Option[Checks] = None,
    thenChecks: Option[Checks] = None,
    elseChecks: Option[Checks] = None
)                                                                           extends NestingCheck
case class PatternCheck(pattern: String)                                    extends SimpleCheck
case class FormatCheck(format: String)                                      extends SimpleCheck
case class MinimumCheck(min: BigDecimal, exclude: Boolean = false)          extends SimpleCheck
case class MinItemsCheck(min: BigDecimal)                                   extends SimpleCheck
case class UniqueItemsCheck(unique: Boolean)                                extends SimpleCheck
case class PropertyNamesCheck(checks: Checks)                               extends NestingCheck
case class DynamicRefCheck(resolve: () => Either[Seq[SchemaError], Checks]) extends NestingCheck
case class MultipleOfCheck(n: Int)                                          extends SimpleCheck
case class MaximumCheck(max: BigDecimal, exclude: Boolean = false)          extends SimpleCheck
case class MaxLengthCheck(max: BigDecimal)                                  extends SimpleCheck
case class MinLengthCheck(min: BigDecimal)                                  extends SimpleCheck

case class Checked[R](valid: Boolean, results: Seq[R], count: Int) {
  def add(others: Seq[Checked[R]]): Checked[R] = Checked(valid, results, count + Checked.count(others))
}

object Checked {
  def apply[R](valid: Boolean, result: R): Checked[R] = Checked[R](valid, Seq(result), 1)
  def valid[R]                                        = Checked[R](true, Seq(), 1)
  def valid[R](result: R)                             = Checked[R](true, Seq(result), 1)
  def invalid[R](result: R)                           = Checked[R](false, Seq(result), 1)
  def merge[R](checked: Seq[Checked[R]]): Checked[R] = {
    val valid           = checked.forall(_.valid)
    val results: Seq[R] = checked.flatMap(_.results)
    Checked(valid, results, 1 + count(checked))
  }
  def count[R](checked: Seq[Checked[R]]): Int = checked.map(_.count).sum
}

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
            .getOrElse(withIgnored(s"${keyword}-${typeName}"))
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
        mapChecksFor(properties) { checks =>
          updateCheck(ObjectPropertiesCheck())(check => check.copy(properties = checks))
        }

      case ("patternProperties", ObjectValue(properties)) =>
        mapChecksFor(properties) { checks =>
          updateCheck(ObjectPropertiesCheck())(check => check.copy(patternProperties = checks))
        }

      case ("additionalProperties", value) =>
        for {
          checks <- Checks.parseKeywords(SchemaValue(value))
        } yield {
          withChecks(checks) { checks =>
            updateCheck(ObjectPropertiesCheck())(check => check.copy(additionalProperties = Some(checks)))
          }
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

      case ("$dynamicAnchor", StringValue(_)) => {
        // handled during load
        Right(this)
      }

      case ("$defs", ObjectValue(_)) => {
        // handled during load
        Right(this)
      }

      case ("$ref", StringValue(ref)) => {
        for {
          resolution <- resolver
            .resolveRef(ref)
            .map(Right(_))
            .getOrElse(Left(Seq(SchemaError(s"""missing reference "${ref}""""))))
          schema    = resolution._1
          resolver1 = resolution._2
          checks <- Checks.parseKeywords(schema)(resolver1)
        } yield {
          withChecks(checks)(identity)
        }
      }

      case ("$dynamicRef", StringValue(ref)) => {
        for {
          resolution <- resolver
            .resolveDynamicRef(ref)
            .map(Right(_))
            .getOrElse(Left(Seq(SchemaError(s"""missing dynamic reference "${ref}""""))))

          resolveLater = { () =>
            val schema    = resolution._1
            val resolver1 = resolution._2
            Checks.parseKeywords(schema)(resolver1)
          }
        } yield {
          withCheck(DynamicRefCheck(resolveLater))
        }
      }

      case ("$comment", StringValue(_)) => {
        // only for schema authors and readers
        Right(this)
      }

      case ("title", StringValue(_)) => {
        // ignore annotations
        Right(this)
      }

      case ("default", _) => {
        // ignore annotations
        Right(this)
      }

      case ("description", StringValue(_)) => {
        // ignore annotations
        Right(this)
      }

      // TODO validation vocabulary
      case ("pattern", StringValue(pattern)) => {
        Right(withCheck(PatternCheck(pattern)))
      }

      // TODO validation vocabulary
      case ("format", StringValue(format)) => {
        Right(withCheck(FormatCheck(format)))
      }

      // TODO validation vocabulary
      case ("minimum", NumberValue(v)) => {
        Right(withCheck(MinimumCheck(v)))
      }

      // TODO validation vocabulary
      case ("exclusiveMinimum", NumberValue(v)) => {
        Right(withCheck(MinimumCheck(v, true)))
      }

      case ("minItems", NumberValue(v)) => {
        Right(withCheck(MinItemsCheck(v)))
      }

      case ("uniqueItems", BoolValue(v)) => {
        Right(withCheck(UniqueItemsCheck(v)))
      }

      case ("propertyNames", value) =>
        for {
          checks <- Checks.parseKeywords(SchemaValue(value))
        } yield {
          withChecks(checks)(c => withCheck(PropertyNamesCheck(c)))
        }

      // TODO validation vocabulary
      case ("multipleOf", NumberValue(v)) if v > 0 => {
        Right(withCheck(MultipleOfCheck(v.toInt)))
      }

      // TODO validation vocabulary
      case ("maximum", NumberValue(v)) => {
        Right(withCheck(MaximumCheck(v)))
      }

      // TODO validation vocabulary
      case ("exclusiveMaximum", NumberValue(v)) => {
        Right(withCheck(MaximumCheck(v, true)))
      }

      // TODO validation vocabulary
      case ("maxLength", NumberValue(v)) if v >= 0 => {
        Right(withCheck(MaxLengthCheck(v)))
      }

      // TODO validation vocabulary
      case ("minLength", NumberValue(v)) if v >= 0 => {
        Right(withCheck(MinLengthCheck(v)))
      }

      case _ => Right(withIgnored(keyword))
    }

  private def mapChecksFor(
      props: Map[String, Value]
  )(f: Map[String, Checks] => Checks)(implicit resolver: SchemaResolver) = {
    val propChecks = props.view
      .mapValues(v => Checks.parseKeywords(SchemaValue(v)))
      .map {
        case (prop, Right(checks)) => Right((prop, checks))
        case (prop, Left(errors))  => Left(errors.map(_.prefix(Pointer.empty / prop)))
      }
      .toSeq
    for {
      propChecks1 <- sequenceFirstLeft(propChecks)
      checks1 = Map.from(propChecks1)
      ignored = checks1.values.flatMap(_.ignoredKeywords).toSet
    } yield {
      f(checks1).withIgnored(ignored)
    }
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

  private def getTypeCheck(typeName: String): Option[TypeCheck] =
    typeName match {
      case "null"    => Some(NullTypeCheck)
      case "boolean" => Some(BooleanTypeCheck)
      case "string"  => Some(StringTypeCheck)
      case "number"  => Some(NumberTypeCheck)
      case "integer" => Some(IntegerTypeCheck)
      case "array"   => Some(ArrayTypeCheck)
      case "object"  => Some(ObjectTypeCheck)
      case _         => None
    }
}

case class Checker[R](
    check: SimpleCheck => Processor.ProcessFun[R],
    nested: NestingCheck => Processor.MergeFun[R]
)

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
