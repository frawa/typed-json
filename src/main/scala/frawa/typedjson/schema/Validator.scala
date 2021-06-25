package frawa.typedjson.schema

import frawa.typedjson.parser.Value
import frawa.typedjson.parser.NullValue
import frawa.typedjson.parser.BoolValue
import frawa.typedjson.parser.StringValue
import frawa.typedjson.parser.NumberValue
import frawa.typedjson.parser.ArrayValue
import frawa.typedjson.parser.ObjectValue

case class ValidationError(reason: ValidationErrorReason, pointer: Pointer = Pointer.empty) {
  def prefix(prefix: Pointer): ValidationError = {
    ValidationError(reason, prefix / pointer)
  }
}

trait ValidationErrorReason
case class TypeMismatch(expected: String)                      extends ValidationErrorReason
case class FalseSchemaReason()                                 extends ValidationErrorReason
case class UnexpectedProperty(key: String)                     extends ValidationErrorReason
case class MissingProperty(key: String)                        extends ValidationErrorReason
case class MissingRef(ref: String)                             extends ValidationErrorReason
case class NotOneOf(valid: Int, results: Seq[ValidationError]) extends ValidationErrorReason
case class NotInvalid()                                        extends ValidationErrorReason

trait ValidationResult {
  val valid: Boolean
  val errors: Seq[ValidationError]
  def and(other: ValidationResult): ValidationResult = ValidationResult.and(this, other)
  def or(other: ValidationResult): ValidationResult  = ValidationResult.or(this, other)
  def prefix(pointer: Pointer)                       = ValidationResult.prefix(this, pointer)
}
case object ValidationValid extends ValidationResult {
  val valid  = true
  val errors = Seq()
}
case class ValidationInvalid(errors: Seq[ValidationError]) extends ValidationResult {
  val valid = false
}

object ValidationResult {
  def apply(errors: Seq[ValidationError]): ValidationResult =
    if (errors.isEmpty) ValidationValid else ValidationInvalid(errors)

  def valid(): ValidationResult                         = ValidationValid
  def invalid(error: ValidationError): ValidationResult = ValidationInvalid(Seq(error))

  def and(a: ValidationResult, b: ValidationResult): ValidationResult = {
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

  def prefix(a: ValidationResult, prefix: Pointer): ValidationResult = {
    if (a.valid) {
      a
    } else {
      ValidationInvalid(a.errors.map(_.prefix(prefix)))
    }
  }
}

trait Validator {
  type Dereferencer = String => Option[Validator]
  def validate(value: Value)(implicit dereference: Dereferencer): ValidationResult
}

object Validator {
  def apply(schema: Schema): Validator = schema match {
    case NullSchema         => NullValidator()
    case TrueSchema         => AlwaysValidator(true)
    case FalseSchema        => AlwaysValidator(false)
    case BooleanSchema      => BooleanValidator()
    case StringSchema       => StringValidator()
    case NumberSchema       => NumberValidator()
    case ArraySchema(items) => ArrayValidator(Validator(items))
    case ObjectSchema(properties) =>
      ObjectValidator(
        properties.view
          .mapValues(Validator(_))
          .toMap
      )
    case RootSchema(_, schema, defs) => RootSchemaValidator(Validator(schema), defs.view.mapValues(Validator(_)).toMap)
    case RefSchema(ref)              => RefValidator(ref)
    case SchemaWithApplicators(schema, allOf, anyOf, oneOf, notOp) =>
      AllOfValidator(
        Seq(
          Validator(schema),
          AllOfValidator(allOf.map(Validator(_))),
          AnyOfValidator(anyOf.map(Validator(_))),
          OneOfValidator(oneOf.map(Validator(_))),
          notOp.map(schema => NotValidator(Validator(schema))).getOrElse(AlwaysValidator(true))
        )
      )
  }

  def validate(validator: Validator)(value: Value): ValidationResult = {
    implicit val dereference: String => Option[Validator] = ref => None
    validator.validate(value)
  }
}

case class NullValidator() extends Validator {
  override def validate(value: Value)(implicit dereference: Dereferencer): ValidationResult = value match {
    case NullValue => ValidationResult.valid()
    case _         => ValidationResult.invalid(ValidationError(TypeMismatch("null")))
  }
}

case class AlwaysValidator(valid: Boolean) extends Validator {
  override def validate(value: Value)(implicit dereference: Dereferencer): ValidationResult = if (valid) {
    ValidationResult.valid()
  } else {
    ValidationResult.invalid(ValidationError(FalseSchemaReason()))
  }
}

case class BooleanValidator() extends Validator {
  override def validate(value: Value)(implicit dereference: Dereferencer): ValidationResult = value match {
    case BoolValue(value) =>
      if (value) ValidationResult.valid() else ValidationResult.invalid(ValidationError(FalseSchemaReason()))
    //
    case _ => ValidationResult.invalid(ValidationError(TypeMismatch("boolean")))
  }
}

case class StringValidator() extends Validator {
  override def validate(value: Value)(implicit dereference: Dereferencer): ValidationResult = value match {
    case StringValue(_) => ValidationResult.valid()
    case _              => ValidationResult.invalid(ValidationError(TypeMismatch("string")))
  }
}

case class NumberValidator() extends Validator {
  override def validate(value: Value)(implicit dereference: Dereferencer): ValidationResult = value match {
    case NumberValue(_) => ValidationResult.valid()
    case _              => ValidationResult.invalid(ValidationError(TypeMismatch("number")))
  }
}

case class ArrayValidator(itemsValidator: Validator) extends Validator {
  override def validate(value: Value)(implicit dereference: Dereferencer): ValidationResult = {
    value match {
      case ArrayValue(items) =>
        items.zipWithIndex
          .map { case (item, index) =>
            lazy val prefix = Pointer.empty / index
            itemsValidator
              .validate(item)
              .prefix(prefix)
          }
          .reduce(_.and(_))
      case _ => ValidationResult.invalid(ValidationError(TypeMismatch("array")))
    }
  }
}

case class ObjectValidator(propertiesValidator: Map[String, Validator]) extends Validator {
  override def validate(value: Value)(implicit dereference: Dereferencer): ValidationResult = {
    value match {
      case ObjectValue(properties) => {
        val validations = properties
          .map { case (key1, value1) =>
            lazy val prefix = Pointer.empty / key1
            propertiesValidator
              .get(key1)
              .map(validator =>
                validator
                  .validate(value1)
                  .prefix(prefix)
              )
              .getOrElse(ValidationResult.invalid(ValidationError(UnexpectedProperty(key1))))
          }
          .toSeq
          .reduce(_.and(_))
        val missing = propertiesValidator.keySet
          .diff(properties.keySet)
          .map(key => ValidationError(MissingProperty(key)))
          .toSeq
        val okOrMissing = ValidationResult(missing)
        validations.and(okOrMissing)
      }
      case _ => ValidationResult.invalid(ValidationError(TypeMismatch("object")))
    }
  }
}

case class RootSchemaValidator(validator: Validator, defs: Map[String, Validator]) extends Validator {
  override def validate(value: Value)(implicit dereference: Dereferencer): ValidationResult =
    validator.validate(value)(dereferenceDefs)

  private def dereferenceDefs(ref: String): Option[Validator] =
    defs.get(relativeize(ref))

  private def relativeize(ref: String): String = if (ref.startsWith("#/$defs/"))
    ref.substring("#/$defs/".length())
  else
    ref
}

case class RefValidator(ref: String) extends Validator {
  override def validate(value: Value)(implicit dereference: Dereferencer): ValidationResult =
    dereference(ref)
      .map(_.validate(value))
      .getOrElse(ValidationResult.invalid(ValidationError(MissingRef(ref))))
}

case class AllOfValidator(vs: Seq[Validator]) extends Validator {
  override def validate(value: Value)(implicit dereference: Dereferencer): ValidationResult =
    vs.map(_.validate(value))
      .reduceOption(_.and(_))
      .getOrElse(ValidationResult.valid())
}

case class AnyOfValidator(vs: Seq[Validator]) extends Validator {
  override def validate(value: Value)(implicit dereference: Dereferencer): ValidationResult =
    vs.map(_.validate(value))
      .reduceOption(_.or(_))
      .getOrElse(ValidationResult.valid())
}

case class OneOfValidator(vs: Seq[Validator]) extends Validator {
  override def validate(value: Value)(implicit dereference: Dereferencer): ValidationResult = {
    if (vs.isEmpty) {
      return ValidationResult.valid()
    }
    val results    = vs.map(_.validate(value))
    val countValid = results.count(_.valid)
    if (countValid == 1) {
      ValidationResult.valid()
    } else {
      ValidationResult.invalid(ValidationError(NotOneOf(countValid, results.flatMap((_.errors)))))
    }
  }
}

case class NotValidator(v: Validator) extends Validator {
  override def validate(value: Value)(implicit dereference: Dereferencer): ValidationResult = {
    val result = v.validate(value)
    if (result.valid) {
      ValidationResult.invalid(ValidationError(NotInvalid()))
    } else {
      ValidationResult.valid()
    }
  }
}
object Helper {
  def debugTraceValue[T](title: String): T => T = { v =>
    println(title, v)
    v
  }
}
