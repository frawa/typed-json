package frawa.typedjson.schema

import frawa.typedjson.parser.Value
import frawa.typedjson.parser.NullValue
import frawa.typedjson.parser.BoolValue
import frawa.typedjson.parser.StringValue
import frawa.typedjson.parser.NumberValue
import frawa.typedjson.parser.ArrayValue
import frawa.typedjson.parser.ObjectValue

case class ValidationError(reason: Reason, pointer: Pointer = Pointer.empty) {
  def prefix(prefix: Pointer): ValidationError = {
    ValidationError(reason, prefix / pointer)
  }
}

trait Reason
case class TypeMismatch(expected: String)  extends Reason
case class FalseSchemaReason()             extends Reason
case class UnexpectedProperty(key: String) extends Reason
case class MissingProperty(key: String)    extends Reason
case class MissingRef(ref: String)         extends Reason
case class NotOneOf(valid: Int)            extends Reason
case class NotInvalid()                    extends Reason

trait ValidationResult {
  val valid: Boolean
  val errors: Seq[ValidationError]
  // def and(other: ValidationResult): ValidationResult = ValidationResult.and(this, other)
  // def or(other: ValidationResult): ValidationResult  = ValidationResult.or(this, other)
  // def prefix(pointer: Pointer)                       = ValidationResult.prefix(this, pointer)
}
case object ValidationValid extends ValidationResult {
  val valid  = true
  val errors = Seq()
}
case class ValidationInvalid(errors: Seq[ValidationError]) extends ValidationResult {
  val valid = false
}

// object ValidationResult {
//   def apply(errors: Seq[ValidationError]): ValidationResult =
//     if (errors.isEmpty) ValidationValid else ValidationInvalid(errors)

//   def valid(): ValidationResult                         = ValidationValid
//   def invalid(error: ValidationError): ValidationResult = ValidationInvalid(Seq(error))

// def and(a: ValidationResult, b: ValidationResult): ValidationResult = {
//   if (a.valid && b.valid) {
//     ValidationValid
//   } else {
//     ValidationInvalid(a.errors ++ b.errors)
//   }
// }

//   def or(a: ValidationResult, b: ValidationResult): ValidationResult = {
//     if (a.valid || b.valid) {
//       ValidationValid
//     } else {
//       ValidationInvalid(a.errors ++ b.errors)
//     }
//   }

//   def prefix(a: ValidationResult, prefix: Pointer): ValidationResult = {
//     if (a.valid) {
//       a
//     } else {
//       ValidationInvalid(a.errors.map(_.prefix(prefix)))
//     }
//   }
// }

object ValidationResultFactory extends EvalResultFactory[ValidationResult] {

  override def valid(): ValidationResult = ValidationValid

  override def invalid(reason: Reason): ValidationResult = ValidationInvalid(
    Seq(ValidationError(reason))
  )

  override def isValid(a: ValidationResult): Boolean = a == ValidationValid

  override def prefix(pointer: Pointer, a: ValidationResult): ValidationResult = {
    if (a.valid) {
      a
    } else {
      ValidationInvalid(a.errors.map(_.prefix(pointer)))
    }
  }

  override def and(a: ValidationResult, b: ValidationResult): ValidationResult = {
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
}

trait Validator {
  type Dereferencer = String => Option[Validator]
  def validate(value: Value)(implicit dereference: Dereferencer): ValidationResult
}

object Validator {
  def validate(schema: Schema)(value: Value): ValidationResult = {
    implicit val dereference: String => Option[Evaluator[ValidationResult]] = ref => None
    Evaluator(schema)(ValidationResultFactory).eval(value)
  }
}

trait EvalResultFactory[R] {
  def valid(): R
  def invalid(reason: Reason): R
  def isValid(a: R): Boolean
  def and(a: R, b: R): R
  def or(a: R, b: R): R
  def prefix(pointer: Pointer, a: R): R
}

trait Evaluator[R] {
  type Dereferencer = String => Option[Evaluator[R]]
  def eval(value: Value)(implicit dereference: Dereferencer): R
}

object Evaluator {
  def apply[R](schema: Schema)(implicit factory: EvalResultFactory[R]): Evaluator[R] = schema match {
    case NullSchema         => NullEvaluator()
    case TrueSchema         => AlwaysEvaluator[R](true)
    case FalseSchema        => AlwaysEvaluator[R](false)
    case BooleanSchema      => BooleanEvaluator[R]()
    case StringSchema       => StringEvaluator[R]()
    case NumberSchema       => NumberEvaluator[R]()
    case ArraySchema(items) => ArrayEvaluator[R](Evaluator[R](items))
    case ObjectSchema(properties) =>
      ObjectEvaluator[R](
        properties.view
          .mapValues(Evaluator[R](_))
          .toMap
      )
    case RootSchema(_, schema, defs) =>
      RootSchemaEvaluator[R](Evaluator[R](schema), defs.view.mapValues(Evaluator[R](_)).toMap)
    case RefSchema(ref) => RefEvaluator[R](ref)
    case SchemaWithApplicators(schema, allOf, anyOf, oneOf, notOp) =>
      AllOfEvaluator(
        Seq(
          Evaluator(schema),
          AllOfEvaluator(allOf.map(Evaluator(_))),
          AnyOfEvaluator(anyOf.map(Evaluator(_))),
          OneOfEvaluator(oneOf.map(Evaluator(_))),
          notOp.map(schema => NotEvaluator(Evaluator(schema))).getOrElse(AlwaysEvaluator(true))
        )
      )
  }
}

case class NullEvaluator[R]()(implicit factory: EvalResultFactory[R]) extends Evaluator[R] {
  override def eval(value: Value)(implicit dereference: Dereferencer): R = value match {
    case NullValue => factory.valid()
    case _         => factory.invalid(TypeMismatch("null"))
  }
}

case class AlwaysEvaluator[R](valid: Boolean)(implicit factory: EvalResultFactory[R]) extends Evaluator[R] {
  override def eval(value: Value)(implicit dereference: Dereferencer): R = if (valid) {
    factory.valid()
  } else {
    factory.invalid(FalseSchemaReason())
  }
}

case class BooleanEvaluator[R]()(implicit factory: EvalResultFactory[R]) extends Evaluator[R] {
  override def eval(value: Value)(implicit dereference: Dereferencer): R = value match {
    case BoolValue(value) =>
      if (value) factory.valid() else factory.invalid(FalseSchemaReason())
    case _ => factory.invalid(TypeMismatch("boolean"))
  }
}

case class StringEvaluator[R]()(implicit factory: EvalResultFactory[R]) extends Evaluator[R] {
  override def eval(value: Value)(implicit dereference: Dereferencer): R = value match {
    case StringValue(_) => factory.valid()
    case _              => factory.invalid(TypeMismatch("string"))
  }
}

case class NumberEvaluator[R]()(implicit factory: EvalResultFactory[R]) extends Evaluator[R] {
  override def eval(value: Value)(implicit dereference: Dereferencer): R = value match {
    case NumberValue(_) => factory.valid()
    case _              => factory.invalid(TypeMismatch("number"))
  }
}

case class ArrayEvaluator[R](itemsEvaluator: Evaluator[R])(implicit factory: EvalResultFactory[R])
    extends Evaluator[R] {
  override def eval(value: Value)(implicit dereference: Dereferencer): R = {
    value match {
      case ArrayValue(items) =>
        items.zipWithIndex
          .map { case (item, index) =>
            lazy val prefix = Pointer.empty / index
            factory.prefix(
              prefix,
              itemsEvaluator.eval(item)
            )
          }
          .reduce(factory.and(_, _))
      case _ => factory.invalid(TypeMismatch("array"))
    }
  }
}

case class ObjectEvaluator[R](propertiesEvaluators: Map[String, Evaluator[R]])(implicit factory: EvalResultFactory[R])
    extends Evaluator[R] {
  override def eval(value: Value)(implicit dereference: Dereferencer): R = {
    value match {
      case ObjectValue(properties) => {
        val validations = properties
          .map { case (key1, value1) =>
            lazy val prefix = Pointer.empty / key1
            propertiesEvaluators
              .get(key1)
              .map(evaluator => factory.prefix(prefix, evaluator.eval(value1)))
              .getOrElse(factory.invalid(UnexpectedProperty(key1)))
          }
          .toSeq
          .foldLeft(factory.valid())(factory.and(_, _))
        val missing = propertiesEvaluators.keySet
          .diff(properties.keySet)
          .map(key => MissingProperty(key))
          .map(factory.invalid(_))
          .toSeq
          .foldLeft(factory.valid())(factory.and(_, _))
        factory.and(validations, missing)
      }
      case _ => factory.invalid(TypeMismatch("object"))
    }
  }
}

case class RootSchemaEvaluator[R](evaluator: Evaluator[R], defs: Map[String, Evaluator[R]])(implicit
    factory: EvalResultFactory[R]
) extends Evaluator[R] {
  override def eval(value: Value)(implicit dereference: Dereferencer): R =
    evaluator.eval(value)(dereferenceDefs)

  private def dereferenceDefs(ref: String): Option[Evaluator[R]] =
    defs.get(relativeize(ref))

  private def relativeize(ref: String): String = if (ref.startsWith("#/$defs/"))
    ref.substring("#/$defs/".length())
  else
    ref
}

case class RefEvaluator[R](ref: String)(implicit factory: EvalResultFactory[R]) extends Evaluator[R] {
  override def eval(value: Value)(implicit dereference: Dereferencer): R =
    dereference(ref)
      .map(_.eval(value))
      .getOrElse(factory.invalid(MissingRef(ref)))
}

case class AllOfEvaluator[R](es: Seq[Evaluator[R]])(implicit factory: EvalResultFactory[R]) extends Evaluator[R] {
  override def eval(value: Value)(implicit dereference: Dereferencer): R =
    es.map(_.eval(value))
      .reduceOption(factory.and(_, _))
      .getOrElse(factory.valid())
}

case class AnyOfEvaluator[R](es: Seq[Evaluator[R]])(implicit factory: EvalResultFactory[R]) extends Evaluator[R] {
  override def eval(value: Value)(implicit dereference: Dereferencer): R =
    es.map(_.eval(value))
      .reduceOption(factory.or(_, _))
      .getOrElse(factory.valid())
}

case class OneOfEvaluator[R](es: Seq[Evaluator[R]])(implicit factory: EvalResultFactory[R]) extends Evaluator[R] {
  override def eval(value: Value)(implicit dereference: Dereferencer): R = {
    if (es.isEmpty) {
      return factory.valid()
    }
    val results    = es.map(_.eval(value))
    val countValid = results.count(factory.isValid(_))
    if (countValid == 1) {
      factory.valid()
    } else {
      factory.invalid(NotOneOf(countValid))
    }
  }
}

case class NotEvaluator[R](e: Evaluator[R])(implicit factory: EvalResultFactory[R]) extends Evaluator[R] {
  override def eval(value: Value)(implicit dereference: Dereferencer): R = {
    val result = e.eval(value)
    if (factory.isValid(result)) {
      factory.invalid(NotInvalid())
    } else {
      factory.valid()
    }
  }
}
object Helper {
  def debugTraceValue[T](title: String): T => T = { v =>
    println(title, v)
    v
  }
}
