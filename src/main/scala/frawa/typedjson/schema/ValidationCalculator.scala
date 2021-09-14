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
  override def allOf(checked: Seq[Checked[ValidationResult]], pointer: Pointer): Checked[ValidationResult] = {
    if (checked.isEmpty || checked.forall(_.valid)) {
      Checked.valid.add(checked)
    } else {
      invalid(checked)
    }
  }

  private def invalid(checked: Seq[Checked[ValidationResult]]): Checked[ValidationResult] =
    Checked.invalid(ValidationResult.invalid(checked.flatMap(_.results.flatMap(_.errors)))).add(checked)

  override def invalid(observation: Observation, pointer: Pointer): Checked[ValidationResult] = Checked.invalid(
    ValidationResult.invalid(observation, pointer)
  )

  override def anyOf(checked: Seq[Checked[ValidationResult]], pointer: Pointer): Checked[ValidationResult] = {
    if (checked.isEmpty || checked.exists(_.valid)) {
      Checked.valid.add(checked)
    } else {
      invalid(checked)
    }
  }

  override def oneOf(checked: Seq[Checked[ValidationResult]], pointer: Pointer): Checked[ValidationResult] = {
    val count = checked.count(_.valid)
    if (count == 1) {
      Checked.valid.add(checked)
    } else if (count == 0) {
      invalid(checked)
    } else {
      invalid(NotOneOf(count), pointer)
    }
  }

  override def not(checked: Seq[Checked[ValidationResult]], pointer: Pointer): Checked[ValidationResult] = {
    if (checked.length == 1 && !checked(0).valid) {
      Checked.valid.add(checked)
    } else {
      invalid(NotInvalid(), pointer)
    }
  }

  override def ifThenElse(
      checked: Seq[Checked[ValidationResult]],
      pointer: Pointer
  ): Checked[ValidationResult] = {
    allOf(checked, pointer)
  }

}
