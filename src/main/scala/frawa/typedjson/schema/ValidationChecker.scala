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

class ValidationChecker() extends Checker[ValidationResult] {
  private val calc: Calculator[ValidationResult] = new ValidationCalculator()

  override def check(checks: Checks)(value: Value): ValidationResult = {
    val results = checks.checks.map(checkOne(_)(value))
    calc.allOf(results)
  }

  private def checkOne(check: Check)(value: Value): ValidationResult =
    check match {
      case NullTypeCheck                                     => checkNullType(value)
      case BooleanTypeCheck                                  => checkBooleanType(value)
      case StringTypeCheck                                   => checkStringType(value)
      case NumberTypeCheck                                   => checkNumberType(value)
      case ArrayTypeCheck                                    => checkArrayType(value)
      case ObjectTypeCheck                                   => checkObjectType(value)
      case ArrayItemsCheck(items)                            => checkArrayItems(items, value)
      case ObjectPropertiesCheck(properties)                 => checkObjectProperties(properties, value)
      case ObjectRequiredCheck(required)                     => checkObjectRequired(required, value)
      case TrivialCheck(valid)                               => checkTrivial(valid)
      case NotCheck(checks)                                  => checkNot(checks, value)
      case AllOfCheck(checks)                                => checkAllOf(checks, value)
      case AnyOfCheck(checks)                                => checkAnyOf(checks, value)
      case OneOfCheck(checks)                                => checkOneOf(checks, value)
      case IfThenElseCheck(ifChecks, thenChecks, elseChecks) => checkIfThenElse(ifChecks, thenChecks, elseChecks, value)
      case UnionTypeCheck(checks)                            => checkUnionType(checks, value)
      case EnumCheck(values)                                 => checkEnum(values, value)
      case _                                                 => ValidationInvalid(Seq(WithPointer(UnsupportedCheck(check))))
    }

  private def checkNullType(value: Value): ValidationResult = value match {
    case NullValue => calc.valid(SchemaValue(NullValue))
    case _         => calc.invalid(TypeMismatch2("null"))
  }

  private def checkBooleanType(value: Value): ValidationResult = value match {
    case BoolValue(_) => calc.valid(SchemaValue(NullValue))
    case _            => calc.invalid(TypeMismatch2("boolean"))
  }

  private def checkStringType(value: Value): ValidationResult = value match {
    case StringValue(_) => calc.valid(SchemaValue(NullValue))
    case _              => calc.invalid(TypeMismatch2("string"))
  }

  private def checkNumberType(value: Value): ValidationResult = value match {
    case NumberValue(_) => calc.valid(SchemaValue(NullValue))
    case _              => calc.invalid(TypeMismatch2("number"))
  }

  private def checkArrayType(value: Value): ValidationResult = value match {
    case ArrayValue(_) => calc.valid(SchemaValue(NullValue))
    case _             => calc.invalid(TypeMismatch2("array"))
  }

  private def checkObjectType(value: Value): ValidationResult = value match {
    case ObjectValue(_) => calc.valid(SchemaValue(NullValue))
    case _              => calc.invalid(TypeMismatch2("object"))
  }

  private def checkTrivial(valid: Boolean): ValidationResult = {
    if (valid)
      calc.valid(SchemaValue(NullValue))
    else
      calc.invalid(FalseSchemaReason())
  }

  private def checkArrayItems(items: Option[Checks], value: Value): ValidationResult = value match {
    case ArrayValue(itemValues) => {
      if (items.isDefined && itemValues.nonEmpty) {
        val processor = items.get.processor(this)
        calc.allOf(
          itemValues.zipWithIndex
            .map { case (item, index) =>
              lazy val prefix = Pointer.empty / index
              lazy val result = processor.process(item)
              calc.prefix(prefix, result)
            }
        )
      } else {
        calc.valid(SchemaValue(NullValue))
      }
    }
    case _ => calc.valid(SchemaValue(NullValue))
  }

  private def checkObjectProperties(properties: Map[String, Checks], value: Value): ValidationResult = value match {
    case ObjectValue(propertiesValues) => {
      val results = propertiesValues.map { case (key1, value1) =>
        lazy val prefix = Pointer.empty / key1
        properties
          .get(key1)
          .map(_.processor(this))
          .map(_.process(value1))
          .map(calc.prefix(prefix, _))
          .getOrElse(calc.invalid(UnexpectedProperty(key1)))
      }.toSeq
      if (properties.isEmpty) {
        calc.valid(SchemaValue(NullValue))
      } else {
        calc.allOf(results)
      }
    }
    case _ => calc.valid(SchemaValue(NullValue))
  }

  private def checkObjectRequired(required: Seq[String], value: Value): ValidationResult = value match {
    case ObjectValue(propertiesValues) => {
      val missingNames = required.filter(!propertiesValues.contains(_))
      if (missingNames.isEmpty) {
        calc.valid(SchemaValue(NullValue))
      } else {
        val missing = missingNames
          .map(name => (name, SchemaValue(NullValue)))
          .toMap
        calc.invalid(MissingProperties2(missing))
      }
    }
    case _ => calc.valid(SchemaValue(NullValue))
  }

  private def checkNot(checks: Checks, value: Value): ValidationResult = {
    val result = checks.processor(this).process(value)
    if (calc.isValid(result))
      calc.invalid(NotInvalid())
    else
      calc.valid(SchemaValue(NullValue))
  }

  private def checkAllOf(checks: Seq[Checks], value: Value): ValidationResult = {
    calc.allOf(checks.map(_.processor(this).process(value)))
  }

  private def checkAnyOf(checks: Seq[Checks], value: Value): ValidationResult = {
    calc.anyOf(checks.map(_.processor(this).process(value)))
  }

  private def checkOneOf(checks: Seq[Checks], value: Value): ValidationResult = {
    calc.oneOf(checks.map(_.processor(this).process(value)))
  }

  private def checkIfThenElse(
      ifChecks: Option[Checks],
      thenChecks: Option[Checks],
      elseChecks: Option[Checks],
      value: Value
  ): ValidationResult = {
    ifChecks
      .map(_.processor(this).process(value))
      .map { result =>
        if (calc.isValid(result)) {
          val thenResult = thenChecks.map(_.processor(this).process(value))
          calc.ifThenElse(result, thenResult, None)
        } else {
          val elseResult = elseChecks.map(_.processor(this).process(value))
          calc.ifThenElse(result, None, elseResult)
        }
      }
      .getOrElse(calc.valid(SchemaValue(NullValue)))
  }

  private def checkUnionType(checks: Seq[Check], value: Value): ValidationResult = {
    calc.oneOf(checks.map(checkOne(_)(value)))
  }

  private def checkEnum(values: Seq[Value], value: Value): ValidationResult = {
    if (values.contains(value)) {
      calc.valid(SchemaValue(NullValue))
    } else {
      calc.invalid(NotInEnum(values))
    }
  }

}
