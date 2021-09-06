package frawa.typedjson.schema

import frawa.typedjson.parser.Value
import frawa.typedjson.parser.NullValue
import frawa.typedjson.parser.BoolValue
import frawa.typedjson.parser.StringValue
import frawa.typedjson.parser.NumberValue
import frawa.typedjson.parser.ArrayValue
import frawa.typedjson.parser.ObjectValue

case class ValidationResult(errors: Seq[ValidationResult.Error])
object ValidationResult {
  type Error = WithPointer[Observation]

  def invalid(errors: Seq[Error]): ValidationResult = ValidationResult(errors)
  def invalid(observation: Observation, pointer: Pointer = Pointer.empty): ValidationResult = invalid(
    Seq(WithPointer(observation, pointer))
  )
}

case class WithPointer[+R](result: R, pointer: Pointer = Pointer.empty)
