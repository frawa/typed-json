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

sealed trait Observation
case class FalseSchemaReason()                        extends Observation
case class TypeMismatch[T <: Value](expected: String) extends Observation
case class UnexpectedProperty(key: String)            extends Observation
case class NotOneOf(valid: Int)                       extends Observation
case class NotInvalid()                               extends Observation
case class NotInEnum(values: Seq[Value])              extends Observation
case class MissingProperties(properties: Seq[String]) extends Observation
case class UnsupportedCheck(check: Check)             extends Observation

trait Calculator[R] {
  def valid(): R
  def isValid(result: R): Boolean
  def invalid(observation: Observation, pointer: Pointer): R
  def allOf(results: Seq[R]): R
  def anyOf(results: Seq[R]): R
  def oneOf(results: Seq[R]): R
//   def not(result: R): R
  def ifThenElse(ifResult: R, thenResult: Option[R], elseResult: Option[R]): R
}
class ValidationChecker() extends Checker[ValidationResult] {
  private val calc: Calculator[ValidationResult] = new ValidationCalculator()

  override def check(checks: Checks)(value: InnerValue): ValidationResult = {
    val results = checks.checks.map(checkOne(_)(value))
    calc.allOf(results)
  }

  private val nullTypeMismatch    = TypeMismatch[NullValue.type]("null")
  private val booleanTypeMismatch = TypeMismatch[BoolValue]("boolean")
  private val stringTypeMismatch  = TypeMismatch[StringValue]("string")
  private val numberTypeMismatch  = TypeMismatch[NumberValue]("number")
  private val arrayTypeMismatch   = TypeMismatch[ArrayValue]("array")
  private val objectTypeMismatch  = TypeMismatch[ObjectValue]("object")

  private def checkOne(check: Check)(value: InnerValue): ValidationResult =
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
      case TrivialCheck(valid)                               => checkTrivial(valid, value)
      case NotCheck(checks)                                  => checkNot(checks, value)
      case AllOfCheck(checks)                                => checkApplicator(calc.allOf)(checks, value)
      case AnyOfCheck(checks)                                => checkApplicator(calc.anyOf)(checks, value)
      case OneOfCheck(checks)                                => checkApplicator(calc.oneOf)(checks, value)
      case IfThenElseCheck(ifChecks, thenChecks, elseChecks) => checkIfThenElse(ifChecks, thenChecks, elseChecks, value)
      case UnionTypeCheck(checks)                            => checkUnionType(checks, value)
      case EnumCheck(values)                                 => checkEnum(values, value)
      case _                                                 => ValidationInvalid(Seq(WithPointer(UnsupportedCheck(check))))
    }

  private def checkType[T <: Value: ClassTag](observation: TypeMismatch[T])(value: InnerValue): ValidationResult =
    value.value match {
      case v: T => calc.valid()
      case _    => calc.invalid(observation, value.pointer)
    }

  private def checkTrivial(valid: Boolean, value: InnerValue): ValidationResult = {
    if (valid)
      calc.valid()
    else
      calc.invalid(FalseSchemaReason(), value.pointer)
  }

  private def checkArrayItems(items: Option[Checks], value: InnerValue): ValidationResult = value.value match {
    case ArrayValue(itemValues) => {
      if (items.isDefined && itemValues.nonEmpty) {
        calc.allOf(
          Processor.processIndexed(this)(items.get)(itemValues, value.pointer)
        )
      } else {
        calc.valid()
      }
    }
    case _ => calc.valid()
  }

  private def checkObjectProperties(properties: Map[String, Checks], value: InnerValue): ValidationResult =
    value.value match {
      case ObjectValue(propertiesValues) => {
        val results = Processor.processMap(this)(properties)(propertiesValues, value.pointer) { case (result, key) =>
          result
            .getOrElse(calc.invalid(UnexpectedProperty(key), value.pointer))
        }
        if (properties.isEmpty) {
          calc.valid()
        } else {
          calc.allOf(results)
        }
      }
      case _ => calc.valid()
    }

  private def checkObjectRequired(required: Seq[String], value: InnerValue): ValidationResult = value.value match {
    case ObjectValue(propertiesValues) => {
      val missingNames = required.filter(!propertiesValues.contains(_))
      if (missingNames.isEmpty) {
        calc.valid()
      } else {
        calc.invalid(MissingProperties(missingNames), value.pointer)
      }
    }
    case _ => calc.valid()
  }

  private def checkNot(checks: Checks, value: InnerValue): ValidationResult = {
    val result = Processor.processor(this)(checks).process(value)
    if (calc.isValid(result))
      calc.invalid(NotInvalid(), value.pointer)
    else
      calc.valid()
  }

  private def checkApplicator(
      f: Seq[ValidationResult] => ValidationResult
  )(checks: Seq[Checks], value: InnerValue): ValidationResult =
    f(checks.map(Processor.processor(this)(_).process(value)))

  private def checkIfThenElse(
      ifChecks: Option[Checks],
      thenChecks: Option[Checks],
      elseChecks: Option[Checks],
      value: InnerValue
  ): ValidationResult = {
    ifChecks
      .map(Processor.processor(this)(_).process(value))
      .map { result =>
        if (calc.isValid(result)) {
          val thenResult = thenChecks.map(Processor.processor(this)(_).process(value))
          calc.ifThenElse(result, thenResult, None)
        } else {
          val elseResult = elseChecks.map(Processor.processor(this)(_).process(value))
          calc.ifThenElse(result, None, elseResult)
        }
      }
      .getOrElse(calc.valid())
  }

  private def checkUnionType(checks: Seq[Check], value: InnerValue): ValidationResult = {
    calc.oneOf(checks.map(checkOne(_)(value)))
  }

  private def checkEnum(values: Seq[Value], value: InnerValue): ValidationResult = {
    if (values.contains(value.value)) {
      calc.valid()
    } else {
      calc.invalid(NotInEnum(values), value.pointer)
    }
  }
}
