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

class ValidationCalculator extends Calculator[ValidationResult] {
  override def valid(): ValidationResult = ValidationValid

  override def invalid(observation: Observation, pointer: Pointer): ValidationResult = ValidationInvalid(
    Seq(WithPointer(observation, pointer))
  )

  override def allOf(checked: Seq[Checked[ValidationResult]], pointer: Pointer): Checked[ValidationResult] = {
    if (checked.isEmpty || checked.forall(_.valid)) {
      Checked(true, Seq())
    } else {
      Checked(false, Seq(ValidationInvalid(checked.flatMap(_.results.flatMap(_.errors)))))
    }
  }

  override def anyOf(checked: Seq[Checked[ValidationResult]], pointer: Pointer): Checked[ValidationResult] = {
    if (checked.isEmpty || checked.exists(_.valid)) {
      Checked(true, Seq())
    } else {
      Checked(false, Seq(ValidationInvalid(checked.flatMap(_.results.flatMap(_.errors)))))
    }
  }

  override def oneOf(checked: Seq[Checked[ValidationResult]], pointer: Pointer): Checked[ValidationResult] = {
    val count = checked.count(_.valid)
    if (count == 1) {
      Checked(true, Seq())
    } else if (count == 0) {
      Checked(false, Seq(ValidationInvalid(checked.flatMap(_.results.flatMap(_.errors)))))
    } else {
      Checked(false, Seq(ValidationInvalid(Seq(WithPointer(NotOneOf(count), pointer)))))
    }
  }

  override def not(checked: Seq[Checked[ValidationResult]], pointer: Pointer): Checked[ValidationResult] = {
    if (checked.length == 1 && !checked(0).valid) {
      Checked(true, Seq())
    } else {
      Checked(false, Seq(ValidationInvalid(Seq(WithPointer(NotInvalid(), pointer)))))
    }
  }

  override def ifThenElse(
      checked: Seq[Checked[ValidationResult]],
      pointer: Pointer
  ): Checked[ValidationResult] = {
    allOf(checked, pointer)
  }

  override def isValid(result: ValidationResult): Boolean = result == ValidationValid

}
