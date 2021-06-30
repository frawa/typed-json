package frawa.typedjson.schema

import frawa.typedjson.parser.Value
import frawa.typedjson.parser.NullValue
import frawa.typedjson.parser.BoolValue
import frawa.typedjson.parser.StringValue
import frawa.typedjson.parser.NumberValue
import frawa.typedjson.parser.ArrayValue
import frawa.typedjson.parser.ObjectValue

case class ValidationError(reason: Reason, pointer: Pointer = Pointer.empty) {
  def prefix(prefix: Pointer): ValidationError = {
    ValidationError(reason, prefix / pointer)
  }
}

trait ValidationResult {
  val valid: Boolean
  val errors: Seq[ValidationError]
}
case object ValidationValid extends ValidationResult {
  val valid  = true
  val errors = Seq()
}
case class ValidationInvalid(errors: Seq[ValidationError]) extends ValidationResult {
  val valid = false
}

object ValidationResultFactory extends EvalResultFactory[ValidationResult] {

  override def valid(): ValidationResult = ValidationValid

  override def invalid(reason: Reason): ValidationResult = ValidationInvalid(
    Seq(ValidationError(reason))
  )

  override def isValid(a: ValidationResult): Boolean = a == ValidationValid

  override def prefix(pointer: Pointer, a: ValidationResult): ValidationResult = {
    if (a.valid) {
      a
    } else {
      ValidationInvalid(a.errors.map(_.prefix(pointer)))
    }
  }

  override def and(a: ValidationResult, b: ValidationResult): ValidationResult = {
    if (a.valid && b.valid) {
      ValidationValid
    } else {
      ValidationInvalid(a.errors ++ b.errors)
    }
  }

  def or(a: ValidationResult, b: ValidationResult): ValidationResult = {
    if (a.valid || b.valid) {
      ValidationValid
    } else {
      ValidationInvalid(a.errors ++ b.errors)
    }
  }
}

trait Validator {
  type Dereferencer = String => Option[Validator]
  def validate(value: Value)(implicit dereference: Dereferencer): ValidationResult
}

object Validator {
  def validate(schema: Schema)(value: Value): ValidationResult = {
    implicit val dereference: String => Option[Evaluator[ValidationResult]] = ref => None
    Evaluator(schema)(ValidationResultFactory).eval(value)
  }
}
