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
import java.util.regex.Pattern
import scala.util.Try

sealed trait Observation
case class FalseSchemaReason()                                extends Observation
case class TypeMismatch[T <: Value](expected: String)         extends Observation
case class NotOneOf(valid: Int)                               extends Observation
case class NotInvalid()                                       extends Observation
case class NotInEnum(values: Seq[Value])                      extends Observation
case class MissingProperties(properties: Seq[String])         extends Observation
case class PatternMismatch(pattern: String)                   extends Observation
case class FormatMismatch(format: String)                     extends Observation
case class MinimumMismatch(min: BigDecimal, exclude: Boolean) extends Observation
case class MinItemsMismatch(min: BigDecimal)                  extends Observation
case class ItemsNotUnique()                                   extends Observation
case class UnsupportedFormat(format: String)                  extends Observation
case class UnsupportedCheck(check: Check)                     extends Observation
case class NotMultipleOf(n: Int)                              extends Observation
case class MaximumMismatch(max: BigDecimal, exclude: Boolean) extends Observation
case class MaxLengthMismatch(max: BigDecimal)                 extends Observation

trait Calculator[R] {
  def invalid(observation: Observation, pointer: Pointer): Checked[R]
  def allOf(checked: Seq[Checked[R]], pointer: Pointer): Checked[R]
  def anyOf(checked: Seq[Checked[R]], pointer: Pointer): Checked[R]
  def oneOf(checked: Seq[Checked[R]], pointer: Pointer): Checked[R]
  def not(checked: Seq[Checked[R]], pointer: Pointer): Checked[R]
  def ifThenElse(checked: Seq[Checked[R]], pointer: Pointer): Checked[R]
}
object ValidationChecker {

  def apply(): Checker[ValidationResult] = Checker(check, nested)

  private val calc: Calculator[ValidationResult] = new ValidationCalculator()

  private val nullTypeMismatch    = TypeMismatch[NullValue.type]("null")
  private val booleanTypeMismatch = TypeMismatch[BoolValue]("boolean")
  private val stringTypeMismatch  = TypeMismatch[StringValue]("string")
  private val numberTypeMismatch  = TypeMismatch[NumberValue]("number")
  private val arrayTypeMismatch   = TypeMismatch[ArrayValue]("array")
  private val objectTypeMismatch  = TypeMismatch[ObjectValue]("object")

  private type ProcessFun = Processor.ProcessFun[ValidationResult]

  private def check(check: SimpleCheck): ProcessFun = {
    check match {
      case NullTypeCheck                 => checkType(nullTypeMismatch)
      case BooleanTypeCheck              => checkType(booleanTypeMismatch)
      case StringTypeCheck               => checkType(stringTypeMismatch)
      case NumberTypeCheck               => checkType(numberTypeMismatch)
      case IntegerTypeCheck              => checkInteger()
      case ArrayTypeCheck                => checkType(arrayTypeMismatch)
      case ObjectTypeCheck               => checkType(objectTypeMismatch)
      case ObjectRequiredCheck(required) => checkObjectRequired(required)
      case TrivialCheck(valid)           => checkTrivial(valid)
      case UnionTypeCheck(checks)        => checkUnionType(checks)
      case EnumCheck(values)             => checkEnum(values)
      case PatternCheck(pattern)         => checkPattern(pattern)
      case FormatCheck(format)           => checkFormat(format)
      case MinimumCheck(v, exclude)      => checkMinimum(v, exclude)
      case MinItemsCheck(v)              => checkMinItems(v)
      case UniqueItemsCheck(v)           => checkUniqueItems(v)
      case MultipleOfCheck(n)            => checkMultipleOf(n)
      case MaximumCheck(v, exclude)      => checkMaximum(v, exclude)
      case MaxLengthCheck(v)             => checkMaxLength(v)
      case _                             => _ => Checked.invalid(ValidationResult.invalid(UnsupportedCheck(check)))
    }
  }

  private def nested(check: NestingCheck)(checked: Seq[Checked[ValidationResult]]): ProcessFun = { value =>
    check match {
      case AllOfCheck(_)                  => calc.allOf(checked, value.pointer)
      case AnyOfCheck(_)                  => calc.anyOf(checked, value.pointer)
      case OneOfCheck(_)                  => calc.oneOf(checked, value.pointer)
      case NotCheck(_)                    => calc.not(checked, value.pointer)
      case ObjectPropertiesCheck(_, _, _) => calc.allOf(checked, value.pointer)
      case ArrayItemsCheck(_)             => calc.allOf(checked, value.pointer)
      case IfThenElseCheck(_, _, _)       => calc.ifThenElse(checked, value.pointer)
      case PropertyNamesCheck(_)          => calc.allOf(checked, value.pointer)
      case DynamicRefCheck(_)             => calc.allOf(checked, value.pointer)
    }
  }

  private def checkType[T <: Value: ClassTag](observation: TypeMismatch[T]): ProcessFun = value =>
    value.value match {
      case v: T => Checked.valid
      case _    => calc.invalid(observation, value.pointer)
    }

  private def checkInteger(): ProcessFun = value =>
    value.value match {
      case NumberValue(v) =>
        if (v.isValidLong)
          Checked.valid
        else
          calc.invalid(TypeMismatch[NumberValue]("integer"), value.pointer)
      case _ => calc.invalid(TypeMismatch[NumberValue]("integer"), value.pointer)
    }

  private def checkTrivial(valid: Boolean): ProcessFun = { value =>
    if (valid)
      Checked.valid
    else
      calc.invalid(FalseSchemaReason(), value.pointer)
  }

  private def checkObjectRequired(required: Seq[String]): ProcessFun = { value =>
    value.value match {
      case ObjectValue(propertiesValues) => {
        val missingNames = required.filter(!propertiesValues.contains(_))
        if (missingNames.isEmpty) {
          Checked.valid
        } else {
          calc.invalid(MissingProperties(missingNames), value.pointer)
        }
      }
      case _ => Checked.valid
    }
  }

  private def checkUnionType(checks: Seq[TypeCheck]): ProcessFun = { value =>
    calc.oneOf(checks.map(check(_)(value)), value.pointer)
  }

  private def checkEnum(values: Seq[Value]): ProcessFun = { value =>
    if (values.contains(value.value)) {
      Checked.valid
    } else {
      calc.invalid(NotInEnum(values), value.pointer)
    }
  }

  private def checkPattern(pattern: String): ProcessFun = { value =>
    value.value match {
      case StringValue(v) =>
        if (v.matches(pattern))
          Checked.valid
        else
          calc.invalid(PatternMismatch(pattern), value.pointer)
      case _ => Checked.valid
    }
  }

  private def checkFormat(format: String): ProcessFun = {
    format match {
      case "regex" =>
        checkStringValue(FormatMismatch(format)) { v =>
          Try(Pattern.compile(v)).isSuccess
        }
      case "uri" =>
        checkStringValue(FormatMismatch(format)) { v =>
          Try(new URI(v)).map(_.isAbsolute).getOrElse(false)
        }
      case "uri-reference" =>
        checkStringValue(FormatMismatch(format)) { v =>
          Try(new URI(v)).isSuccess
        }
      case _ => { value =>
        calc.invalid(UnsupportedFormat(format), value.pointer)
      }
    }
  }

  private def checkStringValue(observation: => Observation)(check: String => Boolean): ProcessFun = { value =>
    value.value match {
      case StringValue(v) =>
        if (check(v))
          Checked.valid
        else
          calc.invalid(observation, value.pointer)
      case _ => Checked.valid
    }
  }

  private def checkMinimum(min: BigDecimal, exclude: Boolean): ProcessFun = {
    checkNumberValue(MinimumMismatch(min, exclude)) { v =>
      if (exclude) min < v else min <= v
    }
  }

  private def checkNumberValue(observation: => Observation)(check: BigDecimal => Boolean): ProcessFun = { value =>
    value.value match {
      case NumberValue(v) =>
        if (check(v))
          Checked.valid
        else
          calc.invalid(observation, value.pointer)
      case _ => Checked.valid
    }
  }

  private def checkMinItems(min: BigDecimal): ProcessFun = {
    checkArrayValue(MinItemsMismatch(min)) { v =>
      min <= v.length
    }
  }

  private def checkArrayValue(observation: => Observation)(check: Seq[Value] => Boolean): ProcessFun = { value =>
    value.value match {
      case ArrayValue(v) =>
        if (check(v))
          Checked.valid
        else
          calc.invalid(observation, value.pointer)
      case _ => Checked.valid
    }
  }

  private def checkUniqueItems(unique: Boolean): ProcessFun = {
    checkArrayValue(ItemsNotUnique()) { v =>
      unique && v.distinct.length == v.length
    }
  }

  private def checkMultipleOf(n: Int): ProcessFun = {
    checkNumberValue(NotMultipleOf(n)) { v =>
      v % n == 0
    }
  }

  private def checkMaximum(max: BigDecimal, exclude: Boolean): ProcessFun = {
    checkNumberValue(MaximumMismatch(max, exclude)) { v =>
      if (exclude) max > v else max >= v
    }
  }

  private def checkMaxLength(max: BigDecimal): ProcessFun = {
    checkStringValue(MaxLengthMismatch(max)) { v =>
      v.length <= max
    }
  }
}
