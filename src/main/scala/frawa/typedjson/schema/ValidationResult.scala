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
