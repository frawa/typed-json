package frawa.typedjson.schema

import munit.FunSuite
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

class ProcessorTest extends FunSuite {
  implicit val zioParser = new ZioParser();

  private def withParsedSchemaValue(text: String, parse: String => Either[String, SchemaValue])(
      f: SchemaValue => Unit
  ) {
    val withSchema = for {
      value <- parse(text)
    } yield {
      f(value)
    }
    withSchema.swap
      .map(message => fail("no schema", clues(clue(message))))
      .swap
  }

  private def withSchema(text: String)(f: SchemaValue => Unit) {
    withParsedSchemaValue(text, SchemaValue.apply)(f)
  }

  private def assertValidate(text: String)(schema: SchemaValue)(
      f: ValidationResult => Unit
  ) = {
    val withParsed = for {
      value <- Parser(text)
      result = Processor.process(new ValidationCalculator())(schema, value)
    } yield {
      f(result)
    }
    withParsed.swap
      .map(message => fail("parsing failed", clues(clue(message))))
      .swap
  }

  test("null") {
    withSchema("""{"type": "null"}""") { schema =>
      assertValidate("""null""")(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate("""13""")(schema) { result =>
        assertEquals(result.errors, Seq(WithPointer(TypeMismatch2("null"))))
        assertEquals(result.valid, false)
      }
    }
  }

  test("boolean") {
    withSchema("""{"type": "boolean"}""") { schema =>
      assertValidate("""true""")(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate("""13""")(schema) { result =>
        assertEquals(result.errors, Seq(WithPointer(TypeMismatch2("boolean"))))
        assertEquals(result.valid, false)
      }
    }
  }

  test("true schema") {
    withSchema("""true""") { schema =>
      assertValidate("""null""")(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate("""13""")(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate("""{}""")(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
    }
  }

  test("false schema") {
    withSchema("""false""") { schema =>
      assertValidate("""null""")(schema) { result =>
        assertEquals(
          result.errors,
          Seq(
            WithPointer(
              result = FalseSchemaReason(),
              pointer = Pointer(
                segments = Nil
              )
            )
          )
        )
        assertEquals(result.valid, false)
      }
      assertValidate("""13""")(schema) { result =>
        assertEquals(
          result.errors,
          Seq(
            WithPointer(
              result = FalseSchemaReason(),
              pointer = Pointer(
                segments = Nil
              )
            )
          )
        )
        assertEquals(result.valid, false)
      }
      assertValidate("""{}""")(schema) { result =>
        assertEquals(
          result.errors,
          Seq(
            WithPointer(
              result = FalseSchemaReason(),
              pointer = Pointer(
                segments = Nil
              )
            )
          )
        )
        assertEquals(result.valid, false)
      }
    }
  }

  test("not false") {
    withSchema("""{"not": false}""") { schema =>
      assertValidate("""null""")(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate("""13""")(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate("""{}""")(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
    }
  }

  test("empty schema") {
    withSchema("""{}""") { schema =>
      assertValidate("""null""")(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate("""13""")(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate("""{}""")(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
    }
  }

  test("not empty") {
    withSchema("""{"not": {}}""") { schema =>
      assertValidate("""null""")(schema) { result =>
        assertEquals(
          result.errors,
          Seq(
            WithPointer(
              result = NotInvalid(),
              pointer = Pointer(
                segments = Nil
              )
            )
          )
        )
        assertEquals(result.valid, false)
      }
      assertValidate("""13""")(schema) { result =>
        assertEquals(
          result.errors,
          Seq(
            WithPointer(
              result = NotInvalid(),
              pointer = Pointer(
                segments = Nil
              )
            )
          )
        )
        assertEquals(result.valid, false)
      }
      assertValidate("""{}""")(schema) { result =>
        assertEquals(
          result.errors,
          Seq(
            WithPointer(
              result = NotInvalid(),
              pointer = Pointer(
                segments = Nil
              )
            )
          )
        )
        assertEquals(result.valid, false)
      }
    }
  }

  test("string") {
    withSchema("""{"type": "string"}""") { schema =>
      assertValidate(""""hello"""")(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate("""13""")(schema) { result =>
        assertEquals(result.errors, Seq(WithPointer(TypeMismatch2("string"))))
      }
    }
  }

  test("number") {
    withSchema("""{"type": "number"}""") { schema =>
      assertValidate("""13""")(schema) { result =>
        assertEquals(result.errors, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate("""null""")(schema) { result =>
        assertEquals(result.errors, Seq(WithPointer(TypeMismatch2("number"))))
      }
    }
  }
}

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
//   def allOf(results: Seq[R]): R
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

case object ArrayHandler extends Handler {}
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
    schema.value match {
      case BoolValue(v) => TrivialHandler(v).handle(calc)(value)
      case ObjectValue(keywords) =>
        val handler1 = keywords
          .foldLeft(handler) { case (handler, (keyword, v)) =>
            handler.withKeyword(keyword, v)
          }
        handler1.handle(calc)(value)
      case _ => calc.invalid(InvalidSchemaValue)
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
}
