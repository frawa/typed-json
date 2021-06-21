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
case class TypeMismatch(expected: String)  extends ValidationErrorReason
case class FalseSchema()                   extends ValidationErrorReason
case class UnexpectedProperty(key: String) extends ValidationErrorReason
case class MissingProperty(key: String)    extends ValidationErrorReason

trait ValidationResult {
  val valid: Boolean
  val errors: Seq[ValidationError]
  def and(other: ValidationResult): ValidationResult = ValidationResult.and(this, other)
}
case object ValidationValid extends ValidationResult {
  val valid  = true
  val errors = Seq()
}
case class ValidationInvalid(errors: Seq[ValidationError]) extends ValidationResult {
  val valid = false
}

object ValidationResult {
  def valid(): ValidationResult                         = ValidationValid
  def invalid(error: ValidationError): ValidationResult = ValidationInvalid(Seq(error))

  def and(a: ValidationResult, b: ValidationResult): ValidationResult = {
    if (a.valid && b.valid) {
      ValidationValid
    } else {
      ValidationInvalid(a.errors ++ b.errors)
    }
  }
}

trait Validator {
  def validate(value: Value): Option[Seq[ValidationError]]
}

object Validator {
  def apply(schema: Schema): Either[String, Validator] = schema match {
    case NullSchema         => Right(NullValidator())
    case BooleanSchema      => Right(BooleanValidator())
    case StringSchema       => Right(StringValidator())
    case NumberSchema       => Right(NumberValidator())
    case ArraySchema(items) => Validator(items).map(ArrayValidator(_))
    case ObjectSchema(properties) =>
      Helper
        .sequence(properties.map { case (key, schema) =>
          Validator(schema).map((key, _))
        }.toSeq)
        .map(_.toMap)
        .map(
          ObjectValidator(_)
        )
  }
}

case class NullValidator() extends Validator {
  override def validate(value: Value): Option[Seq[ValidationError]] = value match {
    case NullValue => None
    case _         => Option(Seq(ValidationError(TypeMismatch("null"))))
  }
}

case class BooleanValidator() extends Validator {
  override def validate(value: Value): Option[Seq[ValidationError]] = value match {
    case BoolValue(value) => if (value) None else Option(Seq(ValidationError(FalseSchema())))
    case _                => Option(Seq(ValidationError(TypeMismatch("boolean"))))
  }
}

case class StringValidator() extends Validator {
  override def validate(value: Value): Option[Seq[ValidationError]] = value match {
    case StringValue(_) => None
    case _              => Option(Seq(ValidationError(TypeMismatch("string"))))
  }
}

case class NumberValidator() extends Validator {
  override def validate(value: Value): Option[Seq[ValidationError]] = value match {
    case NumberValue(_) => None
    case _              => Option(Seq(ValidationError(TypeMismatch("number"))))
  }
}

case class ArrayValidator(itemsValidator: Validator) extends Validator {
  override def validate(value: Value): Option[Seq[ValidationError]] = {
    value match {
      case ArrayValue(items) =>
        Helper
          .sequence(items.zipWithIndex.map { case (item, index) =>
            lazy val prefix = Pointer.empty / index
            itemsValidator
              .validate(item)
              .map(errors => errors.map(_.prefix(prefix)))
          })
          .map(_.flatten)
      case _ => Option(Seq(ValidationError(TypeMismatch("array"))))
    }
  }
}

case class ObjectValidator(propertiesValidator: Map[String, Validator]) extends Validator {
  override def validate(value: Value): Option[Seq[ValidationError]] = {
    value match {
      case ObjectValue(properties) => {
        val validations = properties.map { case (key1, value1) =>
          lazy val prefix = Pointer.empty / key1
          propertiesValidator
            .get(key1)
            .map(validator =>
              validator
                .validate(value1)
                .map(errors => errors.map(_.prefix(prefix)))
            )
            .getOrElse(Option(Seq(ValidationError(UnexpectedProperty(key1)))))
        }.toSeq
        val missing = propertiesValidator.keySet
          .diff(properties.keySet)
          .map(key => ValidationError(MissingProperty(key)))
        val okOrMissing = if (missing.isEmpty) None else Option(missing)
        Helper
          .sequence(validations :+ okOrMissing)
          .map(_.flatten)
      }
      case _ => Option(Seq(ValidationError(TypeMismatch("object"))))
    }
  }
}
object Helper {
  def sequence[T](options: Seq[Option[T]]): Option[Seq[T]] = {
    options.foldLeft(Option.empty[Seq[T]])((acc, v) =>
      v.flatMap(v =>
        acc
          .map(_ :+ v)
          .orElse(Option(Seq(v)))
      ).orElse(acc)
    )
  }
  def sequence[E, T](eithers: Seq[Either[E, T]]): Either[E, Seq[T]] = {
    // TODO continue over Left?
    eithers.foldLeft[Either[E, Seq[T]]](Right[E, Seq[T]](Seq()))((acc, v) => acc.flatMap(acc => v.map(acc :+ _)))
  }

  def debugTraceValue[T](title: String): T => T = { v =>
    println(title, v)
    v
  }
}
