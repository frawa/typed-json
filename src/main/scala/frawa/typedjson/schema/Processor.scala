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

sealed trait Check
case class TrivialCheck(v: Boolean)                               extends Check
case object NullTypeCheck                                         extends Check
case object BooleanTypeCheck                                      extends Check
case object StringTypeCheck                                       extends Check
case object NumberTypeCheck                                       extends Check
case object ArrayTypeCheck                                        extends Check
case class ArrayItemsCheck(items: Option[Checks] = None)          extends Check
case object ObjectTypeCheck                                       extends Check
case class ObjectPropertiesCheck(properties: Map[String, Checks]) extends Check
case class ObjectRequiredCheck(names: Seq[String])                extends Check
case class NotCheck(checks: Checks)                               extends Check
case class AllOfCheck(checks: Seq[Checks])                        extends Check
case class AnyOfCheck(checks: Seq[Checks])                        extends Check
case class OneOfCheck(checks: Seq[Checks])                        extends Check
case class IfThenElseCheck(
    ifChecks: Option[Checks] = None,
    thenChecks: Option[Checks] = None,
    elseChecks: Option[Checks] = None
) extends Check

trait Checker[R] {
  def init: R
  def check(check: Check): Value => R
}

case class Checks(
    schema: SchemaValue,
    checks: Seq[Check] = Seq.empty[Check],
    ignoredKeywords: Set[String] = Set.empty
) {
  //update[C :< Check ]()
  private def withCheck(check: Check): Checks                         = this.copy(checks = checks :+ check)
  private def withChecks(checks: Checks)(f: Checks => Checks): Checks = f(checks).withIgnored(checks.ignoredKeywords)
  private def withChecks(checks: Map[String, Checks])(f: Map[String, Checks] => Check): Checks =
    withCheck(f(checks)).withIgnored(checks.values.flatMap(_.ignoredKeywords).toSet)
  private def withChecks(checks: Seq[Checks])(f: Seq[Checks] => Check): Checks =
    withCheck(f(checks)).withIgnored(checks.flatMap(_.ignoredKeywords).toSet)
  private def withChecks(values: Seq[Value])(f: Seq[Checks] => Check): Either[SchemaErrors, Checks] = {
    val checks0 = values
      .map(v => Checks.parseKeywords(SchemaValue(v)))
      .toSeq
    for {
      checks <- sequence(checks0)
    } yield {
      withChecks(checks)(f)
    }
  }

  type SchemaErrors = Checks.SchemaErrors

  def withKeyword(keyword: String, value: Value): Either[SchemaErrors, Checks] = (keyword, value) match {
    case ("type", StringValue(typeName)) =>
      Right(
        getTypeCheck(typeName)
          .map(withCheck(_))
          .getOrElse(withIgnored(keyword))
      )
    case ("not", value) =>
      for {
        checks <- Checks.parseKeywords(SchemaValue(value))
      } yield {
        withChecks(checks)(c => withCheck(NotCheck(c)))
      }

    case ("items", value) =>
      for {
        checks <- Checks.parseKeywords(SchemaValue(value))
      } yield {
        withChecks(checks) { checks =>
          updateCheck(ArrayItemsCheck())(check => check.copy(items = Some(checks)))
        }
      }

    case ("properties", ObjectValue(properties)) =>
      val propChecks = properties.view
        .mapValues(v => Checks.parseKeywords(SchemaValue(v)))
        .map {
          case (prop, Right(checks)) => Right((prop, checks))
          case (prop, Left(errors))  => Left(errors.map(_.prefix(Pointer.empty / prop)))
        }
        .toSeq
      for {
        propChecks1 <- sequence(propChecks)
        checks = Map.from(propChecks1)
      } yield {
        withChecks(checks)(ObjectPropertiesCheck)
      }

    case ("required", ArrayValue(values)) => {
      def names = values.flatMap(v => Value.asString(v))
      Right(withCheck(ObjectRequiredCheck(names)))
    }

    case ("allOf", ArrayValue(values)) => {
      withChecks(values)(AllOfCheck)
    }

    case ("anyOf", ArrayValue(values)) => {
      withChecks(values)(AnyOfCheck)
    }

    case ("oneOf", ArrayValue(values)) => {
      withChecks(values)(OneOfCheck)
    }

    case ("if", value) =>
      for {
        checks <- Checks.parseKeywords(SchemaValue(value))
      } yield {
        withChecks(checks) { checks =>
          updateCheck(IfThenElseCheck())(check => check.copy(ifChecks = Some(checks)))
        }
      }

    case ("then", value) =>
      for {
        checks <- Checks.parseKeywords(SchemaValue(value))
      } yield {
        withChecks(checks) { checks =>
          updateCheck(IfThenElseCheck())(check => check.copy(thenChecks = Some(checks)))
        }
      }

    case ("else", value) =>
      for {
        checks <- Checks.parseKeywords(SchemaValue(value))
      } yield {
        withChecks(checks) { checks =>
          updateCheck(IfThenElseCheck())(check => check.copy(elseChecks = Some(checks)))
        }
      }

    case _ => Right(withIgnored(keyword))
  }

  // TODO to utils
  private def sequence[V](as: Seq[Either[SchemaErrors, V]]): Either[SchemaErrors, Seq[V]] = {
    as.foldLeft[Either[SchemaErrors, Seq[V]]](Right(Seq.empty[V])) {
      case (Right(acc), Right(v))    => Right(acc :+ v)
      case (Right(_), Left(errors))  => Left(errors)
      case (Left(acc), Left(errors)) => Left(acc :++ errors)
      case (Left(acc), _)            => Left(acc)
    }
  }

  private def updateCheck[C <: Check: ClassTag](newCheck: => C)(f: C => C): Checks = {
    val checks1: Seq[Check] =
      if (
        checks.exists {
          case check: C => true
          case other    => false
        }
      ) {
        checks
      } else {
        checks :+ newCheck
      }
    this.copy(checks = checks1.map {
      case check: C => f(check)
      case other    => other
    })
  }

  private def withIgnored(keyword: String): Checks =
    this.copy(ignoredKeywords = ignoredKeywords + keyword)

  private def withIgnored(ignored: Set[String]): Checks =
    this.copy(ignoredKeywords = ignoredKeywords.concat(ignored))

  private def getTypeCheck(typeName: String): Option[Check] =
    typeName match {
      case "null"    => Some(NullTypeCheck)
      case "boolean" => Some(BooleanTypeCheck)
      case "string"  => Some(StringTypeCheck)
      case "number"  => Some(NumberTypeCheck)
      case "array"   => Some(ArrayTypeCheck)
      case "object"  => Some(ObjectTypeCheck)
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
      if (keywords.isEmpty) {
        Right(Checks(schema).withCheck(TrivialCheck(true)))
      } else {
        keywords
          .foldLeft[Either[SchemaErrors, Checks]](Right(Checks(schema))) { case (checks, (keyword, value)) =>
            val prefix = Pointer.empty / keyword
            checks
              .flatMap(_.withKeyword(keyword, value))
              .swap
              .map(_.map(_.prefix(prefix)))
              .swap
          }
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
