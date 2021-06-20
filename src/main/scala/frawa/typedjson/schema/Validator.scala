package frawa.typedjson.schema

import frawa.typedjson.parser.Value
import frawa.typedjson.parser.NullValue
import frawa.typedjson.parser.BoolValue
import frawa.typedjson.parser.StringValue
import frawa.typedjson.parser.NumberValue
import frawa.typedjson.parser.ArrayValue

case class Error(error: ValidationError, pointer: Pointer = Pointer.empty) {
  def prefix(prefix: Pointer): Error = {
    Error(error, prefix / pointer)
  }
}

trait ValidationError
case class TypeMismatch(expected: String) extends ValidationError
case class FalseSchema()                  extends ValidationError

trait Validator {
  def validate(value: Value): Option[Seq[Error]]
}

object Validator {
  def apply(schema: Schema): Either[String, Validator] = schema match {
    case NullSchema         => Right(NullValidator())
    case BooleanSchema      => Right(BooleanValidator())
    case StringSchema       => Right(StringValidator())
    case NumberSchema       => Right(NumberValidator())
    case ArraySchema(items) => Validator(items).map(ArrayValidator(_))
  }
}

case class NullValidator() extends Validator {
  override def validate(value: Value): Option[Seq[Error]] = value match {
    case NullValue => None
    case _         => Option(Seq(Error(TypeMismatch("null"))))
  }
}

case class BooleanValidator() extends Validator {
  override def validate(value: Value): Option[Seq[Error]] = value match {
    case BoolValue(value) => if (value) None else Option(Seq(Error(FalseSchema())))
    case _                => Option(Seq(Error(TypeMismatch("boolean"))))
  }
}

case class StringValidator() extends Validator {
  override def validate(value: Value): Option[Seq[Error]] = value match {
    case StringValue(_) => None
    case _              => Option(Seq(Error(TypeMismatch("string"))))
  }
}

case class NumberValidator() extends Validator {
  override def validate(value: Value): Option[Seq[Error]] = value match {
    case NumberValue(_) => None
    case _              => Option(Seq(Error(TypeMismatch("number"))))
  }
}

case class ArrayValidator(itemValidator: Validator) extends Validator {
  override def validate(value: Value): Option[Seq[Error]] = {
    value match {
      case ArrayValue(items) =>
        Helper
          .sequence(items.zipWithIndex.map { case (item, index) =>
            lazy val prefix = Pointer.empty / index
            itemValidator
              .validate(item)
              .map(errors => errors.map(_.prefix(prefix)))
          })
          .map(_.flatten)
      case _ => Option(Seq(Error(TypeMismatch("array"))))
    }
  }
}

object Helper {
  def sequence[T](options: Seq[Option[T]]): Option[Seq[T]] = {
    options.foldLeft(Option(Seq.empty[T]))((acc, v) => acc.flatMap(acc => v.map(acc :+ _)))
  }
}
