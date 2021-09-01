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
case class NotOneOf(valid: Int)                       extends Observation
case class NotInvalid()                               extends Observation
case class NotInEnum(values: Seq[Value])              extends Observation
case class MissingProperties(properties: Seq[String]) extends Observation
case class UnsupportedCheck(check: Check)             extends Observation

trait Calculator[R] {
  def valid(): R
  def isValid(result: R): Boolean
  def invalid(observation: Observation, pointer: Pointer): R
  def allOf(checked: Seq[Checked[R]], pointer: Pointer): Checked[R]
  def anyOf(checked: Seq[Checked[R]], pointer: Pointer): Checked[R]
  def oneOf(checked: Seq[Checked[R]], pointer: Pointer): Checked[R]
  def not(checked: Seq[Checked[R]], pointer: Pointer): Checked[R]
  // def ifThenElse(ifResult: R, thenResult: Option[R], elseResult: Option[R]): R
}
object ValidationChecker {

  def apply(): Checker[ValidationResult] = Checker(check, applicate)

  private val calc: Calculator[ValidationResult] = new ValidationCalculator()

  private val nullTypeMismatch    = TypeMismatch[NullValue.type]("null")
  private val booleanTypeMismatch = TypeMismatch[BoolValue]("boolean")
  private val stringTypeMismatch  = TypeMismatch[StringValue]("string")
  private val numberTypeMismatch  = TypeMismatch[NumberValue]("number")
  private val arrayTypeMismatch   = TypeMismatch[ArrayValue]("array")
  private val objectTypeMismatch  = TypeMismatch[ObjectValue]("object")

  private def check(check: SimpleCheck)(value: InnerValue): Checked[ValidationResult] = {
    check match {
      case NullTypeCheck                 => toChecked(checkType(nullTypeMismatch)(value))
      case BooleanTypeCheck              => toChecked(checkType(booleanTypeMismatch)(value))
      case StringTypeCheck               => toChecked(checkType(stringTypeMismatch)(value))
      case NumberTypeCheck               => toChecked(checkType(numberTypeMismatch)(value))
      case ArrayTypeCheck                => toChecked(checkType(arrayTypeMismatch)(value))
      case ObjectTypeCheck               => toChecked(checkType(objectTypeMismatch)(value))
      case ObjectRequiredCheck(required) => toChecked(checkObjectRequired(required, value))
      case TrivialCheck(valid)           => toChecked(checkTrivial(valid, value))
      case UnionTypeCheck(checks)        => checkUnionType(checks, value)
      case EnumCheck(values)             => toChecked(checkEnum(values, value))
      case _                             => toChecked(ValidationInvalid(Seq(WithPointer(UnsupportedCheck(check)))))
    }
  }

  private def applicate(
      kind: ApplicatorKind
  )(checked: Seq[Checked[ValidationResult]])(value: InnerValue): Checked[ValidationResult] = {
    kind match {
      case AllOfKind => calc.allOf(checked, value.pointer)
      case AnyOfKind => calc.anyOf(checked, value.pointer)
      case OneOfKind => calc.oneOf(checked, value.pointer)
      case NotKind   => calc.not(checked, value.pointer)
    }
  }

  private def toChecked(result: ValidationResult): Checked[ValidationResult] =
    Checked(calc.isValid(result), Seq(result))

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

  // private def checkedNot(checked: Checked[ValidationResult], value: InnerValue): ValidationResult = {
  //   if (checked.valid)
  //     calc.invalid(NotInvalid(), value.pointer)
  //   else
  //     calc.valid()
  // }

  // private def checkedApplicator(
  //     f: Seq[Checked[ValidationResult]] => ValidationResult
  // )(checked: Seq[Checked[ValidationResult]], value: InnerValue): ValidationResult =
  //   f(checked)

  // private def checkIfThenElse(
  //     ifChecks: Option[Checks],
  //     thenChecks: Option[Checks],
  //     elseChecks: Option[Checks],
  //     value: InnerValue
  // ): ValidationResult = {
  //   ifChecks
  //     .map(Processor.processor(this)(_).process(value))
  //     .map { result =>
  //       if (calc.isValid(result)) {
  //         val thenResult = thenChecks.map(Processor.processor(this)(_).process(value))
  //         calc.ifThenElse(result, thenResult, None)
  //       } else {
  //         val elseResult = elseChecks.map(Processor.processor(this)(_).process(value))
  //         calc.ifThenElse(result, None, elseResult)
  //       }
  //     }
  //     .getOrElse(calc.valid())
  // }

  private def checkUnionType(checks: Seq[TypeCheck], value: InnerValue): Checked[ValidationResult] = {
    calc.oneOf(checks.map(check(_)(value)), value.pointer)
  }

  private def checkEnum(values: Seq[Value], value: InnerValue): ValidationResult = {
    if (values.contains(value.value)) {
      calc.valid()
    } else {
      calc.invalid(NotInEnum(values), value.pointer)
    }
  }
}
