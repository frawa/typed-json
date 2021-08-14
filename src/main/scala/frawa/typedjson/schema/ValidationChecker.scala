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
  override def init: ValidationResult            = ValidationValid

  override def check(check: Check): Value => ValidationResult = { value: Value =>
    check match {
      case NullTypeCheck          => checkNull(value)
      case BooleanTypeCheck       => checkBoolean(value)
      case StringTypeCheck        => checkString(value)
      case NumberTypeCheck        => checkNumber(value)
      case ArrayTypeCheck         => checkArray(value)
      case ArrayItemsCheck(items) => checkArrayItems(items, value)
      case TrivialCheck(valid)    => checkTrivial(valid)
      case NotCheck(checks)       => checkNot(checks, value)
      case _                      => ValidationInvalid(Seq(WithPointer(UnsupportedCheck(check))))
    }
  }

  override def aggregate(results: Seq[ValidationResult]): ValidationResult = calc.allOf(results)

  private def checkNull(value: Value): ValidationResult = value match {
    case NullValue => calc.valid(SchemaValue(NullValue))
    case _         => calc.invalid(TypeMismatch2("null"))
  }

  private def checkBoolean(value: Value): ValidationResult = value match {
    case BoolValue(_) => calc.valid(SchemaValue(NullValue))
    case _            => calc.invalid(TypeMismatch2("boolean"))
  }

  private def checkString(value: Value): ValidationResult = value match {
    case StringValue(_) => calc.valid(SchemaValue(NullValue))
    case _              => calc.invalid(TypeMismatch2("string"))
  }

  private def checkNumber(value: Value): ValidationResult = value match {
    case NumberValue(_) => calc.valid(SchemaValue(NullValue))
    case _              => calc.invalid(TypeMismatch2("number"))
  }

  private def checkArray(value: Value): ValidationResult = value match {
    case ArrayValue(_) => calc.valid(SchemaValue(NullValue))
    case _             => calc.invalid(TypeMismatch2("array"))
  }

  private def checkTrivial(valid: Boolean): ValidationResult = {
    if (valid)
      calc.valid(SchemaValue(NullValue))
    else
      calc.invalid(FalseSchemaReason())
  }

  private def checkNot(checks: Checks, value: Value): ValidationResult = {
    val result = checks.processor(this).process(value)
    if (calc.isValid(result))
      calc.invalid(NotInvalid())
    else
      calc.valid(SchemaValue(NullValue))
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

}
