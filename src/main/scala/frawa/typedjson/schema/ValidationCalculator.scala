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
case class UnexpectedProperty(key: String)            extends Observation
case class NotOneOf(valid: Int)                       extends Observation
case class NotInvalid()                               extends Observation
case class NotInEnum(values: Seq[Value])              extends Observation
case class MissingProperties(properties: Seq[String]) extends Observation
case class UnsupportedCheck(check: Check)             extends Observation

case class WithPointer[+R](result: R, pointer: Pointer = Pointer.empty) {
  def prefix(prefix: Pointer): WithPointer[R] = WithPointer(result, prefix / pointer)
  def map[S](f: R => S)                       = WithPointer(f(result), pointer)
}

class ValidationCalculator extends Calculator[ValidationResult] {
  override def valid(): ValidationResult = ValidationValid

  override def invalid(observation: Observation): ValidationResult = ValidationInvalid(
    Seq(WithPointer(observation))
  )

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

  override def isValid(result: ValidationResult): Boolean = result == ValidationValid

  override def ifThenElse(
      ifResult: ValidationResult,
      thenResult: Option[ValidationResult],
      elseResult: Option[ValidationResult]
  ): ValidationResult = {
    if (isValid(ifResult)) {
      thenResult.getOrElse(valid())
    } else {
      elseResult.getOrElse(valid())
    }
  }
}
