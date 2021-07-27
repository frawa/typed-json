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
  def withKeyword(keyword: String, value: Value): Option[Handler] = None
  def handle[R](calc: Calculator[R])(value: Value): R             = calc.invalid(NotHandled(this))
}

case class HandlerError(reason: String)                             extends Observation
case class NotHandled(handler: Handler)                             extends Observation
case object InvalidSchemaValue                                      extends Observation
case class TypeMismatch2(expected: String)                          extends Observation
case class MissingProperties2(properties: Map[String, SchemaValue]) extends Observation

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

case class RootHandler(schema: SchemaValue) extends Handler {

  private val core = new CoreHandler(schema)

  override def withKeyword(keyword: String, value: Value): Option[Handler] = keyword match {
    case _ => core.withKeyword(keyword, value)
  }

  override def handle[R](calc: Calculator[R])(value: Value): R = core.handle(calc)(value)
}

case class CoreHandler(schema: SchemaValue, handlers: Seq[Handler] = Seq.empty) extends Handler {
  override def withKeyword(keyword: String, value: Value): Option[Handler] = {
    (keyword, value) match {
      case ("type", StringValue(typeName)) => getTypeHandler(typeName, schema).flatMap(add(_))
      case ("type", ArrayValue(typeNames)) =>
        add(UnionHandler(Utils.toStrings(typeNames).flatMap(getTypeHandler(_, schema))))
      case ("not", value)                 => add(NotHandler(SchemaValue(value)))
      case ("allOf", ArrayValue(schemas)) => add(AllOfHandler(schemas.map(SchemaValue(_))))
      case ("anyOf", ArrayValue(schemas)) => add(AnyOfHandler(schemas.map(SchemaValue(_))))
      case ("oneOf", ArrayValue(schemas)) => add(OneOfHandler(schemas.map(SchemaValue(_))))
      case ("if", value) =>
        firstHandler(keyword, value)
          .orElse(add(IfThenElseHandler(Some(SchemaValue(value)))))
      case ("then", value) =>
        firstHandler(keyword, value)
          .orElse(add(IfThenElseHandler(None, Some(SchemaValue(value)))))
      case ("else", value) =>
        firstHandler(keyword, value)
          .orElse(add(IfThenElseHandler(None, None, Some(SchemaValue(value)))))
      case ("enum", ArrayValue(values)) => add(EnumHandler(this, values))
      case _ =>
        firstHandler(keyword, value)
          .orElse(Some(ErroredHandler(s"""unhandled keyword "${keyword}": ${value}""")))
    }
  }

  private def getTypeHandler(typeName: String, schema: SchemaValue): Option[Handler] = {
    typeName match {
      case "null"    => Some(NullHandler(schema))
      case "boolean" => Some(BooleanHandler(schema))
      case "string"  => Some(StringHandler(schema))
      case "array"   => Some(ArrayHandler(schema))
      case "number"  => Some(NumberHandler(schema))
      case "object"  => Some(ObjectHandler(schema))
      case _         => None
    }
  }

  private def add(handler: Handler): Option[Handler] = {
    Some(CoreHandler(schema, handlers :+ handler))
  }

  private def firstHandler(keyword: String, value: Value): Option[Handler] = {
    Processor.firstHandler(handlers)(keyword, value)
  }

  override def handle[R](calc: Calculator[R])(value: Value): R =
    if (handlers.isEmpty) {
      calc.valid(SchemaValue(NullValue))
    } else {
      calc.allOf(handlers.map(handler => handler.handle(calc)(value)))
    }
}

case class TrivialHandler(valid: Boolean) extends Handler {
  override def handle[R](calc: Calculator[R])(value: Value): R =
    if (valid)
      calc.valid(SchemaValue(NullValue))
    else
      calc.invalid(FalseSchemaReason())
}

case class ErroredHandler(reason: String) extends Handler {
  override def withKeyword(keyword: String, value: Value): Option[Handler] = Some(this)
  override def handle[R](calc: Calculator[R])(value: Value): R             = calc.invalid(HandlerError(reason))
}

case class NullHandler(schema: SchemaValue) extends Handler {
  override def handle[R](calc: Calculator[R])(value: Value): R = {
    value match {
      case NullValue => calc.valid(schema)
      case _         => calc.invalid(TypeMismatch2("null"))
    }
  }
}

case class BooleanHandler(schema: SchemaValue) extends Handler {
  override def handle[R](calc: Calculator[R])(value: Value): R = {
    value match {
      case BoolValue(v) => calc.valid(schema)
      case _            => calc.invalid(new TypeMismatch2("boolean"))
    }
  }
}

case class NotHandler(schema: SchemaValue) extends Handler {
  override def handle[R](calc: Calculator[R])(value: Value): R = {
    val result = Processor.process(CoreHandler(schema), calc)(schema, value)
    if (calc.isValid(result))
      calc.invalid(NotInvalid())
    else
      calc.valid(schema)
  }
}
case class StringHandler(schema: SchemaValue) extends Handler {
  override def handle[R](calc: Calculator[R])(value: Value): R = {
    value match {
      case StringValue(v) => calc.valid(schema)
      case _              => calc.invalid(new TypeMismatch2("string"))
    }
  }
}
case class NumberHandler(schema: SchemaValue) extends Handler {
  override def handle[R](calc: Calculator[R])(value: Value): R = {
    value match {
      case NumberValue(v) => calc.valid(schema)
      case _              => calc.invalid(new TypeMismatch2("number"))
    }
  }
}

case class ArrayHandler(schema: SchemaValue) extends Handler {
  override def withKeyword(keyword: String, value: Value): Option[Handler] = (keyword, value) match {
    case ("items", value) => Some(ArrayItemsHandler(SchemaValue(value)))
    case _                => None
  }

  override def handle[R](calc: Calculator[R])(value: Value): R = {
    value match {
      case ArrayValue(vs) => calc.valid(schema)
      case _              => calc.invalid(new TypeMismatch2("array"))
    }
  }
}

case class ArrayItemsHandler(schema: SchemaValue) extends Handler {
  override def handle[R](calc: Calculator[R])(value: Value): R = {
    value match {
      case ArrayValue(items) =>
        val handler = Processor.getHandler(CoreHandler(schema))(schema)
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

case class ObjectHandler(
    schema: SchemaValue,
    propertySchemas: Map[String, Value] = Map.empty,
    required: Seq[String] = Seq.empty
) extends Handler {
  override def withKeyword(keyword: String, value: Value): Option[Handler] = (keyword, value) match {
    case ("properties", ObjectValue(properties)) => Some(ObjectHandler(schema, properties))
    case ("required", ArrayValue(names))         => Some(ObjectHandler(schema, propertySchemas, requiredNames(names)))
    case _                                       => None
  }

  private def requiredNames(names: Seq[Value]): Seq[String] = Utils.toStrings(names)

  override def handle[R](calc: Calculator[R])(value: Value): R = {
    value match {
      case ObjectValue(properties) =>
        val results = properties.map { case (key1, value1) =>
          lazy val prefix = Pointer.empty / key1
          propertySchemas
            .get(key1)
            .map(SchemaValue(_))
            .map(schema => Processor.getHandler(CoreHandler(schema))(schema))
            .map(handler => handler.handle(calc)(value1))
            .map(calc.prefix(prefix, _))
            .getOrElse(calc.invalid(UnexpectedProperty(key1)))
        }.toSeq
        val missingNames = required
          .filter(!properties.contains(_))
        if (missingNames.isEmpty) {
          calc.allOf(results)
        } else {
          val missing = propertySchemas.view
            .filterKeys(missingNames.contains(_))
            .mapValues(SchemaValue(_))
            .toMap
          calc.allOf(results :+ calc.invalid(MissingProperties2(missing)))
        }
      case _ => calc.invalid(new TypeMismatch2("object"))
    }
  }
}

case class AllOfHandler(schemas: Seq[SchemaValue]) extends Handler {
  override def handle[R](calc: Calculator[R])(value: Value): R = {
    calc.allOf(
      schemas.map(schema => Processor.process(calc)(schema, value))
    )
  }
}

case class AnyOfHandler(schemas: Seq[SchemaValue]) extends Handler {
  override def handle[R](calc: Calculator[R])(value: Value): R = {
    calc.anyOf(
      schemas.map(schema => Processor.process(calc)(schema, value))
    )
  }
}

case class OneOfHandler(schemas: Seq[SchemaValue]) extends Handler {
  override def handle[R](calc: Calculator[R])(value: Value): R = {
    calc.oneOf(
      schemas.map(schema => Processor.process(calc)(schema, value))
    )
  }
}

case class IfThenElseHandler(
    ifSchema: Option[SchemaValue],
    thenSchema: Option[SchemaValue] = None,
    elseSchema: Option[SchemaValue] = None
) extends Handler {
  override def withKeyword(keyword: String, value: Value): Option[Handler] = (keyword, value) match {
    case ("if", value)   => Some(IfThenElseHandler(Some(SchemaValue(value)), thenSchema, elseSchema))
    case ("then", value) => Some(IfThenElseHandler(ifSchema, Some(SchemaValue(value))))
    case ("else", value) => Some(IfThenElseHandler(ifSchema, thenSchema, Some(SchemaValue(value))))
    case _               => None
  }

  override def handle[R](calc: Calculator[R])(value: Value): R = {
    ifSchema
      .map(Processor.process(calc)(_, value))
      .map { result =>
        if (calc.isValid(result)) {
          val thenResult = thenSchema.map(Processor.process(calc)(_, value))
          calc.ifThenElse(result, thenResult, None)
        } else {
          val elseResult = elseSchema.map(Processor.process(calc)(_, value))
          calc.ifThenElse(result, None, elseResult)
        }
      }
      .getOrElse(calc.valid(ifSchema.getOrElse(SchemaValue(NullValue))))
  }
}

case class UnionHandler(handlers: Seq[Handler]) extends Handler {
  override def withKeyword(keyword: String, value: Value): Option[Handler] =
    Processor.firstHandler(handlers)(keyword, value)
  override def handle[R](calc: Calculator[R])(value: Value): R = {
    calc.oneOf(
      handlers.map(_.handle(calc)(value))
    )
  }
}

case class EnumHandler(handler: Handler, values: Seq[Value]) extends Handler {
  override def handle[R](calc: Calculator[R])(value: Value): R = {
    val result = handler.handle(calc)(value)
    if (calc.isValid(result) && values.contains(value)) {
      result
    } else {
      calc.invalid(NotInEnum(values))
    }
  }
}

object Processor {
  def process[R](calc: Calculator[R])(schema: SchemaValue, value: Value): R = {
    process(RootHandler(schema), calc)(schema, value)
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
            handler
              .withKeyword(keyword, v)
              .getOrElse(ErroredHandler(s"""invalid schema ${schema} with keyword "${keyword}"": ${v}"""))
          }
      case _ => ErroredHandler(s"invalid schema ${schema}")
    }
  }

  def firstHandler(handlers: Seq[Handler])(keyword: String, value: Value): Option[Handler] = {
    val fw = handlers
      .to(LazyList)
      .flatMap(_.withKeyword(keyword, value))
      .headOption
    println("FW", keyword, fw)
    fw
  }
}

object Utils {
  def toStrings(values: Seq[Value]): Seq[String] = values.flatMap {
    case StringValue(v) => Some(v)
    case _              => None
  }
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
