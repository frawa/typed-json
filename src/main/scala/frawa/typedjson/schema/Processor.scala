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

case class SchemaValue(value: Value)

object SchemaValue {
  def apply(json: String)(implicit parser: Parser): Either[String, SchemaValue] =
    for {
      value <- parser.parse(json)
    } yield SchemaValue(value)
}

trait Handler {
  def withKeyword(keyword: String, value: Value): Handler = this
  def handle[R](calc: Calculator[R])(value: Value): R     = calc.invalid(NotHandled(this))
}

case class HandlerError(reason: String)    extends Observation
case class NotHandled(handler: Handler)    extends Observation
case object InvalidSchemaValue             extends Observation
case class TypeMismatch2(expected: String) extends Observation

trait Calculator[R] {
  def valid(schema: SchemaValue): R
  def invalid(observation: Observation): R
  def prefix(prefix: Pointer, result: R): R
  def allOf(results: Seq[R]): R
//   def anyOf(results: Seq[R]): R
//   def oneOf(results: Seq[R]): R
//   def not(result: R): R
//   def ifThenElse(ifResult: R, thenResult: R, elseResult: R): R
}

case object RootHandler extends Handler {
  override def withKeyword(keyword: String, value: Value): Handler = keyword match {
    case _ => CoreHandler.withKeyword(keyword, value)
  }
  override def handle[R](calc: Calculator[R])(value: Value): R = CoreHandler.handle(calc)(value)
}

case object CoreHandler extends Handler {
  override def withKeyword(keyword: String, value: Value): Handler = {
    (keyword, value) match {
      case ("type", StringValue("null"))    => NullHandler
      case ("type", StringValue("boolean")) => BooleanHandler
      case ("type", StringValue("string"))  => StringHandler
      case ("type", StringValue("number"))  => NumberHandler
      case ("type", StringValue("array"))   => ArrayHandler
      case ("type", StringValue("object"))  => ObjectHandler
      case ("not", value)                   => NotHandler(SchemaValue(value))
      case _                                => ErroredHandler(s"""unexpected keyword "${keyword}": ${value}""")
    }
  }
  override def handle[R](calc: Calculator[R])(value: Value): R = calc.valid(SchemaValue(NullValue))
}

case class TrivialHandler(valid: Boolean) extends Handler {
  override def handle[R](calc: Calculator[R])(value: Value): R =
    if (valid)
      calc.valid(SchemaValue(NullValue))
    else
      calc.invalid(FalseSchemaReason())
}
case class ErroredHandler(reason: String) extends Handler {
  override def withKeyword(keyword: String, value: Value): Handler = this
  override def handle[R](calc: Calculator[R])(value: Value): R     = calc.invalid(HandlerError(reason))
}

case object NullHandler extends Handler {
  override def handle[R](calc: Calculator[R])(value: Value): R = {
    value match {
      case NullValue => calc.valid(SchemaValue(NullValue))
      case _         => calc.invalid(TypeMismatch2("null"))
    }
  }
}

case object BooleanHandler extends Handler {
  override def handle[R](calc: Calculator[R])(value: Value): R = {
    value match {
      case BoolValue(v) => calc.valid(SchemaValue(NullValue))
      case _            => calc.invalid(new TypeMismatch2("boolean"))
    }
  }
}

case class NotHandler(schema: SchemaValue) extends Handler {
  override def handle[R](calc: Calculator[R])(value: Value): R = {
    val result = Processor.process(CoreHandler, calc)(schema, value)
    if (result == calc.valid(schema))
      calc.invalid(NotInvalid())
    else
      calc.valid(schema)
  }
}
case object StringHandler extends Handler {
  override def handle[R](calc: Calculator[R])(value: Value): R = {
    value match {
      case StringValue(v) => calc.valid(SchemaValue(NullValue))
      case _              => calc.invalid(new TypeMismatch2("string"))
    }
  }
}
case object NumberHandler extends Handler {
  override def handle[R](calc: Calculator[R])(value: Value): R = {
    value match {
      case NumberValue(v) => calc.valid(SchemaValue(NullValue))
      case _              => calc.invalid(new TypeMismatch2("number"))
    }
  }
}

case object ArrayHandler extends Handler {
  override def withKeyword(keyword: String, value: Value): Handler = (keyword, value) match {
    case ("items", value) => ArrayItemsHandler(SchemaValue(value))
    case _                => ErroredHandler(s"""unexpected keyword "${keyword}": ${value}""")
  }

  override def handle[R](calc: Calculator[R])(value: Value): R = {
    value match {
      case ArrayValue(vs) => calc.valid(SchemaValue(NullValue))
      case _              => calc.invalid(new TypeMismatch2("array"))
    }
  }
}

case class ArrayItemsHandler(schema: SchemaValue) extends Handler {
  override def handle[R](calc: Calculator[R])(value: Value): R = {
    value match {
      case ArrayValue(items) =>
        val handler = Processor.getHandler(CoreHandler)(schema)
        calc.allOf(
          items.zipWithIndex
            .map { case (item, index) =>
              lazy val prefix = Pointer.empty / index
              lazy val result = handler.handle(calc)(item)
              calc.prefix(prefix, result)
            }
        )
      case _ => calc.invalid(new TypeMismatch2("array"))
    }
  }
}

case object ObjectHandler extends Handler {
  override def withKeyword(keyword: String, value: Value): Handler = (keyword, value) match {
    case ("properties", ObjectValue(properties)) => PropertiesHandler(properties)
    case _                                       => ErroredHandler(s"unexpected keyword ${keyword} ${value}")
  }
}

case class PropertiesHandler(properties: Map[String, Value]) extends Handler {}

object Processor {
  def process[R](calc: Calculator[R])(schema: SchemaValue, value: Value): R = {
    process(RootHandler, calc)(schema, value)
  }

  def process[R](handler: Handler, calc: Calculator[R])(schema: SchemaValue, value: Value): R = {
    getHandler(handler)(schema).handle(calc)(value)
  }

  def getHandler[R](handler: Handler)(schema: SchemaValue): Handler = {
    schema.value match {
      case BoolValue(v) => TrivialHandler(v)
      case ObjectValue(keywords) =>
        keywords
          .foldLeft(handler) { case (handler, (keyword, v)) =>
            handler.withKeyword(keyword, v)
          }
      case _ => ErroredHandler(s"invalid schema ${schema}")
    }
  }
}

// ResultCalculator[ValidationResult]
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

  private def isValid(result: ValidationResult): Boolean = result == ValidationValid

}
