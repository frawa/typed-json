package frawa.typedjson.schema

import frawa.typedjson.parser.Value
import frawa.typedjson.parser.NullValue
import frawa.typedjson.parser.BoolValue
import frawa.typedjson.parser.StringValue
import frawa.typedjson.parser.NumberValue
import frawa.typedjson.parser.ArrayValue
import frawa.typedjson.parser.ObjectValue

trait ValidationResult {
  val valid: Boolean
  val errors: Seq[ValidationResult.Error]
  def prefix(prefix: Pointer): ValidationResult
}

object ValidationResult {
  type Error = WithPointer[Observation]
}

case object ValidationValid extends ValidationResult {
  val valid                                     = true
  val errors                                    = Seq()
  def prefix(prefix: Pointer): ValidationResult = this
}
case class ValidationInvalid(errors: Seq[ValidationResult.Error]) extends ValidationResult {
  val valid                                     = false
  def prefix(prefix: Pointer): ValidationResult = ValidationInvalid(errors.map(_.prefix(prefix)))
}

object ValidationResultCalculator extends ResultCalculator[ValidationResult] {
  override def valid(schema: Schema): ValidationResult = ValidationValid

  override def invalid(observation: Observation): ValidationResult = ValidationInvalid(
    Seq(WithPointer(observation))
  )

  private def isValid(result: ValidationResult): Boolean = result == ValidationValid

  override def prefix(prefix: Pointer, result: ValidationResult): ValidationResult = result.prefix(prefix)

  override def allOf(results: Seq[ValidationResult]): ValidationResult = {
    if (results.isEmpty || results.forall(isValid(_))) {
      ValidationValid
    } else {
      ValidationInvalid(results.flatMap(_.errors))
    }
  }

  override def anyOf(results: Seq[ValidationResult]): ValidationResult = {
    if (results.isEmpty || results.exists(isValid(_))) {
      ValidationValid
    } else {
      ValidationInvalid(results.flatMap(_.errors))
    }
  }

  override def oneOf(results: Seq[ValidationResult]): ValidationResult = {
    val count = results.count(isValid(_))
    if (count == 1) {
      ValidationValid
    } else if (count == 0) {
      ValidationInvalid(results.flatMap(_.errors))
    } else {
      ValidationInvalid(Seq(WithPointer(NotOneOf(count))))
    }
  }

  override def not(result: ValidationResult): ValidationResult = {
    if (!isValid(result)) {
      ValidationValid
    } else {
      ValidationInvalid(Seq(WithPointer(NotInvalid())))
    }
  }

  override def ifThenElse(
      ifResult: ValidationResult,
      thenResult: ValidationResult,
      elseResult: ValidationResult
  ): ValidationResult = {
    if (isValid(ifResult)) {
      thenResult
    } else {
      elseResult
    }
  }
}

trait Validator {
  type Dereferencer = String => Option[Validator]
  def validate(value: Value)(implicit dereference: Dereferencer): ValidationResult
}

object Validator {
  def validate(schema: Schema)(value: Value): ValidationResult = {
    Evaluator(schema)(value)(ValidationResultCalculator)
  }
}
