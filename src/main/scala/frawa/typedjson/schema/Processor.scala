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

case class SchemaValue(value: Value)

object SchemaValue {
  def apply(json: String)(implicit parser: Parser): Either[String, SchemaValue] =
    for {
      value <- parser.parse(json)
    } yield SchemaValue(value)
}

trait SchemaResolver {
  val base: Option[URI]                      = None
  def resolve(uri: URI): Option[SchemaValue] = None

  def resolveRef(ref: String): Option[SchemaValue] = {
    def uri = URI.create(ref)
    if (uri.isAbsolute()) {
      resolve(uri)
    } else if (uri.getFragment.startsWith("/")) {
      val pointer = Pointer.parse(uri.getFragment())
      base
        .flatMap(resolve(_))
        .flatMap(resolvePointer(_, pointer))
    } else {
      base
        .map(_.resolve(uri))
        .flatMap(resolve(_))
    }
  }

  private def resolvePointer(schema: SchemaValue, pointer: Pointer): Option[SchemaValue] =
    pointer(schema.value).map(SchemaValue(_))
}

case object RootSchemaResolver extends SchemaResolver {
  override def resolve(uri: URI): Option[SchemaValue] = None
}

case class RelativeSchemaResolver(id: String, resolver: SchemaResolver) extends SchemaResolver {
  override val base                                   = Some(URI.create(id).normalize())
  override def resolve(uri: URI): Option[SchemaValue] = resolver.resolve(uri)
}
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

trait Check
case class TrivialCheck(v: Boolean)     extends Check
case class NullCheck()                  extends Check
case class BooleanCheck()               extends Check
case class StringCheck()                extends Check
case class NumberCheck()                extends Check
case class ArrayCheck()                 extends Check
case class ObjectCheck()                extends Check
case class NotCheck(checks: Seq[Check]) extends Check

trait Checker[R] {
  def init: R
  def check(check: Check): Value => R
}

case class Checks(
    schama: SchemaValue,
    checks: Seq[Check] = Seq.empty[Check],
    ignoredKeywords: Map[String, Value] = Map.empty
) {
  //update[C :< Check ]()
  def withCheck(check: Check): Checks = this.copy(checks = checks :+ check)

  type SchemaErrors = Checks.SchemaErrors

  def withKeyword(keyword: String, value: Value): Either[SchemaErrors, Checks] = (keyword, value) match {
    case ("type", StringValue(typeName)) =>
      Right(
        getTypeCheck(typeName)
          .map(withCheck(_))
          .getOrElse(withIgnored(keyword, value))
      )
    case ("not", value) =>
      for {
        checks <- Checks.parseKeywords(SchemaValue(value))
      } yield {
        withCheck(NotCheck(checks.checks)).withIgnored(checks.ignoredKeywords)
      }

    case _ => Right(withIgnored(keyword, value))
  }

  private def withIgnored(keyword: String, value: Value): Checks =
    this.copy(ignoredKeywords = ignoredKeywords + ((keyword, value)))

  private def withIgnored(ignored: Map[String, Value]): Checks =
    this.copy(ignoredKeywords = ignoredKeywords.concat(ignored))

  private def getTypeCheck(typeName: String): Option[Check] =
    typeName match {
      case "null"    => Some(NullCheck())
      case "boolean" => Some(BooleanCheck())
      case "string"  => Some(StringCheck())
      case "number"  => Some(NumberCheck())
      case "array"   => Some(ArrayCheck())
      case "object"  => Some(ObjectCheck())
      case _         => None
    }

  def processor[R](checker: Checker[R]): Processor[R] = Processor(value =>
    checks.foldLeft(checker.init) { case (result, check) =>
      checker.check(check)(value)
    }
  )
}

case class SchemaError(message: String, pointer: Pointer = Pointer.empty) {
  def prefix(prefix: Pointer): SchemaError = SchemaError(message, prefix / pointer)
}

object Checks {
  type SchemaErrors = Seq[SchemaError]

  def parseKeywords(schema: SchemaValue): Either[SchemaErrors, Checks] = schema.value match {
    case BoolValue(v) => Right(Checks(schema).withCheck(TrivialCheck(v)))
    case ObjectValue(keywords) =>
      keywords
        .foldLeft[Either[SchemaErrors, Checks]](Right(Checks(schema))) { case (checks, (keyword, value)) =>
          val prefix = Pointer.empty / keyword
          checks
            .flatMap(_.withKeyword(keyword, value))
            .swap
            .map(_.map(_.prefix(prefix)))
            .swap
        }
    case _ => Left(Seq(SchemaError(s"invalid schema ${schema}")))
  }
}

case class Processor[R] private[schema] (process: Value => R)

object Processor {
  type SchemaErrors = Checks.SchemaErrors

  def apply[R](schema: SchemaValue)(checker: Checker[R]): Either[SchemaErrors, Processor[R]] = for {
    checks <- Checks.parseKeywords(schema)
  } yield (checks.processor(checker))

  // def process[R](handler: Handler, calc: Calculator[R])(
  //     schema: SchemaValue,
  //     value: Value
  // ): R = {
  //   getHandler(handler)(schema).handle(calc)(value)
  // }

  // def getHandler[R](handler: Handler)(schema: SchemaValue): Handler = {
  //   schema.value match {
  //     case BoolValue(v) => TrivialHandler(v)
  //     case ObjectValue(keywords) =>
  //       keywords
  //         .foldLeft(handler) { case (handler, (keyword, v)) =>
  //           handler
  //             .withKeyword(keyword, v)
  //             .getOrElse(ErroredHandler(s"""unhandled schema with keyword "${keyword}": ${v}, handler ${handler}"""))
  //         }
  //     case _ => ErroredHandler(s"invalid schema ${schema}")
  //   }
  // }

  // def firstHandler(handlers: Seq[Handler])(keyword: String, value: Value): (Option[Handler], Seq[Handler]) = {
  //   // handlers
  //   //   .to(LazyList)
  //   //   .flatMap(_.withKeyword(keyword, value))
  //   //   .headOption
  //   handlers.foldLeft((Option.empty[Handler], Seq.empty[Handler])) { case ((first, handlers), current) =>
  //     if (first.isDefined) {
  //       (first, handlers :+ current)
  //     } else {
  //       current
  //         .withKeyword(keyword, value)
  //         .map(h => (Some(h), handlers :+ h))
  //         .getOrElse((Option.empty[Handler], handlers :+ current))
  //     }
  //   }
  // }
}

object Utils {
  def toStrings(values: Seq[Value]): Seq[String] = values.flatMap {
    case StringValue(v) => Some(v)
    case _              => None
  }
}

case class HandlerError(reason: String) extends Observation
// case class NotHandled(handler: Handler)                             extends Observation
case object InvalidSchemaValue                                      extends Observation
case class TypeMismatch2(expected: String)                          extends Observation
case class MissingProperties2(properties: Map[String, SchemaValue]) extends Observation

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
