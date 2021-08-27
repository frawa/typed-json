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
}

object ValidationResult {
  type Error = WithPointer[Observation]
}

case class WithPointer[+R](result: R, pointer: Pointer = Pointer.empty)

case object ValidationValid extends ValidationResult {
  val valid  = true
  val errors = Seq()
}
case class ValidationInvalid(errors: Seq[ValidationResult.Error]) extends ValidationResult {
  val valid = false
}
