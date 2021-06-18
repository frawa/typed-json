package frawa.typedjson.schema

import frawa.typedjson.parser.Value
import frawa.typedjson.parser.NullValue
import frawa.typedjson.parser.BoolValue
import frawa.typedjson.parser.StringValue
import frawa.typedjson.parser.NumberValue
import frawa.typedjson.parser.ArrayValue

trait Error
case class TypeError(expected: String) extends Error

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
    case _         => Option(Seq(TypeError("null")))
  }
}

case class BooleanValidator() extends Validator {
  override def validate(value: Value): Option[Seq[Error]] = value match {
    case BoolValue(_) => None
    case _            => Option(Seq(TypeError("boolean")))
  }
}

case class StringValidator() extends Validator {
  override def validate(value: Value): Option[Seq[Error]] = value match {
    case StringValue(_) => None
    case _              => Option(Seq(TypeError("string")))
  }
}

case class NumberValidator() extends Validator {
  override def validate(value: Value): Option[Seq[Error]] = value match {
    case NumberValue(_) => None
    case _              => Option(Seq(TypeError("number")))
  }
}

case class ArrayValidator(itemValidator: Validator) extends Validator {
  override def validate(value: Value): Option[Seq[Error]] = {
    value match {
      case ArrayValue(items) => Helper.sequence(items.map(item => itemValidator.validate(item))).map(_.flatten)
      case _                 => Option(Seq(TypeError("array")))
    }
  }
}

object Helper {
  def sequence[T](options: Seq[Option[T]]): Option[Seq[T]] = {
    options.foldLeft(Option(Seq.empty[T]))((acc, v) => acc.flatMap(acc => v.map(acc :+ _)))
  }
}
