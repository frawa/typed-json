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

case class SchemaValue(value: Value)

object SchemaValue {
  def apply(json: String)(implicit parser: Parser): Either[String, SchemaValue] =
    for {
      value <- parser.parse(json)
    } yield SchemaValue(value)
}

// case object RootSchemaResolver extends SchemaResolver {
//   override def resolve(uri: URI): Option[SchemaValue] = None
// }

// case class RelativeSchemaResolver(id: String, resolver: SchemaResolver) extends SchemaResolver {
//   override val base                                   = Some(URI.create(id).normalize())
//   override def resolve(uri: URI): Option[SchemaValue] = resolver.resolve(uri)
// }

trait Calculator[R] {
  def valid(schema: SchemaValue): R
  def isValid(result: R): Boolean
  def invalid(observation: Observation): R
  def prefix(prefix: Pointer, result: R): R
  def allOf(results: Seq[R]): R
  def anyOf(results: Seq[R]): R
  def oneOf(results: Seq[R]): R
//   def not(result: R): R
  def ifThenElse(ifResult: R, thenResult: Option[R], elseResult: Option[R]): R
}

case class Processor[R] private[schema] (process: Value => R)

object Processor {
  type SchemaErrors = Checks.SchemaErrors

  def apply[R](
      schema: SchemaValue
  )(checker: Checker[R]): Either[SchemaErrors, Processor[R]] = {
    implicit val resolver = LoadedSchemasResolver(schema)
    for {
      checks <- Checks.parseKeywords(schema)
    } yield (checks.processor(checker))
  }
}

sealed trait Observation
// case class TypeMismatch(expected: Schema)                     extends Observation
case class FalseSchemaReason()             extends Observation
case class UnexpectedProperty(key: String) extends Observation
// case class MissingProperties(properties: Map[String, Schema]) extends Observation
case class MissingRef(ref: String)       extends Observation
case class NotOneOf(valid: Int)          extends Observation // ??? only in Validator?
case class NotInvalid()                  extends Observation // ??? only in Validator?
case class NotInEnum(values: Seq[Value]) extends Observation
case class HandlerError(reason: String)  extends Observation
// case class NotHandled(handler: Handler)                             extends Observation
case object InvalidSchemaValue                                      extends Observation
case class TypeMismatch2(expected: String)                          extends Observation
case class MissingProperties2(properties: Map[String, SchemaValue]) extends Observation
case class UnsupportedCheck(check: Check)                           extends Observation

case class WithPointer[+R](result: R, pointer: Pointer = Pointer.empty) {
  def prefix(prefix: Pointer): WithPointer[R] = WithPointer(result, prefix / pointer)
  def map[S](f: R => S)                       = WithPointer(f(result), pointer)
}

class ValidationCalculator extends Calculator[ValidationResult] {
  override def valid(schema: SchemaValue): ValidationResult = ValidationValid

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
      thenResult.getOrElse(valid(SchemaValue(NullValue)))
    } else {
      elseResult.getOrElse(valid(SchemaValue(NullValue)))
    }
  }
}
