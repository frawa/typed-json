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

trait Calculator[R] {
  def valid(): R
  def isValid(result: R): Boolean
  def invalid(observation: Observation): R
  def prefix(prefix: Pointer, result: R): R
  def allOf(results: Seq[R]): R
  def anyOf(results: Seq[R]): R
  def oneOf(results: Seq[R]): R
//   def not(result: R): R
  def ifThenElse(ifResult: R, thenResult: Option[R], elseResult: Option[R]): R
}
class ValidationChecker() extends Checker[ValidationResult] {
  private val calc: Calculator[ValidationResult] = new ValidationCalculator()

  override def check(checks: Checks)(value: Value): ValidationResult = {
    val results = checks.checks.map(checkOne(_)(value))
    calc.allOf(results)
  }

  private val nullTypeMismatch    = TypeMismatch[NullValue.type]("null")
  private val booleanTypeMismatch = TypeMismatch[BoolValue]("boolean")
  private val stringTypeMismatch  = TypeMismatch[StringValue]("string")
  private val numberTypeMismatch  = TypeMismatch[NumberValue]("number")
  private val arrayTypeMismatch   = TypeMismatch[ArrayValue]("array")
  private val objectTypeMismatch  = TypeMismatch[ObjectValue]("object")

  private def checkOne(check: Check)(value: Value): ValidationResult =
    check match {
      case NullTypeCheck                                     => checkType(nullTypeMismatch)(value)
      case BooleanTypeCheck                                  => checkType(booleanTypeMismatch)(value)
      case StringTypeCheck                                   => checkType(stringTypeMismatch)(value)
      case NumberTypeCheck                                   => checkType(numberTypeMismatch)(value)
      case ArrayTypeCheck                                    => checkType(arrayTypeMismatch)(value)
      case ObjectTypeCheck                                   => checkType(objectTypeMismatch)(value)
      case ArrayItemsCheck(items)                            => checkArrayItems(items, value)
      case ObjectPropertiesCheck(properties)                 => checkObjectProperties(properties, value)
      case ObjectRequiredCheck(required)                     => checkObjectRequired(required, value)
      case TrivialCheck(valid)                               => checkTrivial(valid)
      case NotCheck(checks)                                  => checkNot(checks, value)
      case AllOfCheck(checks)                                => checkApplicator(calc.allOf)(checks, value)
      case AnyOfCheck(checks)                                => checkApplicator(calc.anyOf)(checks, value)
      case OneOfCheck(checks)                                => checkApplicator(calc.oneOf)(checks, value)
      case IfThenElseCheck(ifChecks, thenChecks, elseChecks) => checkIfThenElse(ifChecks, thenChecks, elseChecks, value)
      case UnionTypeCheck(checks)                            => checkUnionType(checks, value)
      case EnumCheck(values)                                 => checkEnum(values, value)
      case _                                                 => ValidationInvalid(Seq(WithPointer(UnsupportedCheck(check))))
    }

  private def checkType[T <: Value: ClassTag](observation: TypeMismatch[T])(value: Value): ValidationResult =
    value match {
      case v: T => calc.valid()
      case _    => calc.invalid(observation)
    }

  private def checkTrivial(valid: Boolean): ValidationResult = {
    if (valid)
      calc.valid()
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
        calc.valid()
      }
    }
    case _ => calc.valid()
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
        calc.valid()
      } else {
        calc.allOf(results)
      }
    }
    case _ => calc.valid()
  }

  private def checkObjectRequired(required: Seq[String], value: Value): ValidationResult = value match {
    case ObjectValue(propertiesValues) => {
      val missingNames = required.filter(!propertiesValues.contains(_))
      if (missingNames.isEmpty) {
        calc.valid()
      } else {
        calc.invalid(MissingProperties(missingNames))
      }
    }
    case _ => calc.valid()
  }

  private def checkNot(checks: Checks, value: Value): ValidationResult = {
    val result = checks.processor(this).process(value)
    if (calc.isValid(result))
      calc.invalid(NotInvalid())
    else
      calc.valid()
  }

  private def checkApplicator(
      f: Seq[ValidationResult] => ValidationResult
  )(checks: Seq[Checks], value: Value): ValidationResult =
    f(checks.map(_.processor(this).process(value)))

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
      .getOrElse(calc.valid())
  }

  private def checkUnionType(checks: Seq[Check], value: Value): ValidationResult = {
    calc.oneOf(checks.map(checkOne(_)(value)))
  }

  private def checkEnum(values: Seq[Value], value: Value): ValidationResult = {
    if (values.contains(value)) {
      calc.valid()
    } else {
      calc.invalid(NotInEnum(values))
    }
  }
}
