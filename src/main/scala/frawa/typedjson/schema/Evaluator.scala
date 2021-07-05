package frawa.typedjson.schema

import frawa.typedjson.parser.Value
import frawa.typedjson.parser.NullValue
import frawa.typedjson.parser.BoolValue
import frawa.typedjson.parser.StringValue
import frawa.typedjson.parser.NumberValue
import frawa.typedjson.parser.ArrayValue
import frawa.typedjson.parser.ObjectValue

trait Observation
case class TypeMismatch(expected: Schema)                     extends Observation
case class FalseSchemaReason()                                extends Observation
case class UnexpectedProperty(key: String)                    extends Observation
case class MissingProperties(properties: Map[String, Schema]) extends Observation
case class MissingRef(ref: String)                            extends Observation
case class NotOneOf(valid: Int)                               extends Observation // ??? only in Validator?
case class NotInvalid()                                       extends Observation // ??? only in Validator?
case class NotInEnum(values: Seq[Value])                      extends Observation

trait ResultCalculator[R] {
  def valid(schema: Schema): R
  def invalid(observation: Observation): R
  def prefix(prefix: Pointer, result: R): R
  def allOf(results: Seq[R]): R
  def anyOf(results: Seq[R]): R
  def oneOf(results: Seq[R]): R
  def not(result: R): R
  def ifThenElse(ifResult: R, thenResult: R, elseResult: R): R
}

case class WithPointer[+R](result: R, pointer: Pointer = Pointer.empty) {
  def prefix(prefix: Pointer): WithPointer[R] = WithPointer(result, prefix / pointer)
  def map[S](f: R => S)                       = WithPointer(f(result), pointer)
}
trait Evaluator {
  type Dereferencer = String => Option[Evaluator]
  def eval[R](value: Value)(calc: ResultCalculator[R])(implicit dereference: Dereferencer): R

  def apply[R](value: Value)(calc: ResultCalculator[R]) = {
    implicit val dereference: String => Option[Evaluator] = ref => None
    this.eval(value)(calc)
  }
}

object Evaluator {

  def apply[R](schema: Schema): Evaluator = {
    schema match {
      case NullSchema                        => NullEvaluator(schema)
      case TrueSchema                        => AlwaysEvaluator(true)
      case FalseSchema                       => AlwaysEvaluator(false)
      case BooleanSchema                     => BooleanEvaluator(schema)
      case StringSchema                      => StringEvaluator(schema)
      case NumberSchema                      => NumberEvaluator(schema)
      case UnionSchema(schemas: Seq[Schema]) => OneOfEvaluator(schemas.map(Evaluator(_)))
      case ArraySchema(items)                => ArrayEvaluator(schema)(Evaluator(items))
      case ObjectSchema(properties)          => ObjectEvaluator(schema)(properties)
      case RootSchema(_, schema, defs) =>
        RootSchemaEvaluator(
          Evaluator(schema),
          defs.view
            .mapValues(Evaluator(_))
            .toMap
        )
      case RefSchema(ref) => RefEvaluator(ref)
      case SchemaWithApplicators(schema, applicators) =>
        AllOfEvaluator(
          Seq(
            Evaluator(schema),
            applicators.allOf.map(_.map(Evaluator(_))).map(AllOfEvaluator(_)).getOrElse(AlwaysEvaluator(true)),
            applicators.anyOf.map(_.map(Evaluator(_))).map(AnyOfEvaluator(_)).getOrElse(AlwaysEvaluator(true)),
            applicators.oneOf.map(_.map(Evaluator(_))).map(OneOfEvaluator(_)).getOrElse(AlwaysEvaluator(true)),
            applicators.notOp.map(schema1 => NotEvaluator(Evaluator(schema1))).getOrElse(AlwaysEvaluator(true)),
            applicators.ifThenElse
              .map(ifThenElse =>
                IfThenElseEvaluator(
                  Evaluator(ifThenElse.ifSchema),
                  Evaluator(ifThenElse.thenSchema),
                  Evaluator(ifThenElse.elseSchema)
                )
              )
              .getOrElse(AlwaysEvaluator(true))
          )
        )
      case s @ SchemaWithValidators(schema, validators) =>
        AllOfEvaluator(
          Seq(
            validators.`enum`.map(EnumEvaluator(s)(Evaluator(schema), _)).getOrElse(AlwaysEvaluator(true)),
            validators.`const`
              .map(c => EnumEvaluator(s)(Evaluator(schema), Seq(c)))
              .getOrElse(AlwaysEvaluator(true))
          )
        )
    }
  }
}

case class NullEvaluator(schema: Schema) extends Evaluator {
  override def eval[R](value: Value)(calc: ResultCalculator[R])(implicit dereference: Dereferencer): R =
    value match {
      case NullValue => calc.valid(schema)
      case _         => calc.invalid(TypeMismatch(schema))
    }
}

case class AlwaysEvaluator(valid: Boolean) extends Evaluator {
  override def eval[R](value: Value)(calc: ResultCalculator[R])(implicit dereference: Dereferencer): R = if (valid) {
    calc.valid(TrueSchema)
  } else {
    calc.invalid(FalseSchemaReason())
  }
}

case class BooleanEvaluator(schema: Schema) extends Evaluator {
  override def eval[R](value: Value)(calc: ResultCalculator[R])(implicit dereference: Dereferencer): R =
    value match {
      case BoolValue(value) => if (value) calc.valid(schema) else calc.invalid(FalseSchemaReason())
      case _                => calc.invalid(TypeMismatch(schema))
    }
}

case class StringEvaluator(schema: Schema) extends Evaluator {
  override def eval[R](value: Value)(calc: ResultCalculator[R])(implicit dereference: Dereferencer): R =
    value match {
      case StringValue(_) => calc.valid(schema)
      case _              => calc.invalid(TypeMismatch(schema))
    }
}

case class NumberEvaluator(schema: Schema) extends Evaluator {
  override def eval[R](value: Value)(calc: ResultCalculator[R])(implicit dereference: Dereferencer): R =
    value match {
      case NumberValue(_) => calc.valid(schema)
      case _              => calc.invalid(TypeMismatch(schema))
    }
}

case class ArrayEvaluator(schema: Schema)(itemsEvaluator: Evaluator) extends Evaluator {
  override def eval[R](value: Value)(calc: ResultCalculator[R])(implicit dereference: Dereferencer): R = {
    value match {
      case ArrayValue(items) =>
        calc.allOf(
          items.zipWithIndex
            .map { case (item, index) =>
              lazy val prefix = Pointer.empty / index
              calc.prefix(prefix, itemsEvaluator.eval(item)(calc))
            }
        )
      case _ => calc.invalid(TypeMismatch(schema))
    }
  }
}

case class ObjectEvaluator(schema: Schema)(schemaByProperty: Map[String, Schema]) extends Evaluator {
  override def eval[R](value: Value)(calc: ResultCalculator[R])(implicit dereference: Dereferencer): R = {
    value match {
      case ObjectValue(properties) => {
        val validations = properties.map { case (key1, value1) =>
          lazy val prefix = Pointer.empty / key1
          schemaByProperty
            .get(key1)
            .map(Evaluator(_))
            .map(_.eval(value1)(calc))
            .map(calc.prefix(prefix, _))
            .getOrElse(calc.invalid(UnexpectedProperty(key1)))
        }.toSeq
        val missing = schemaByProperty.view
          .filterKeys(!properties.contains(_))
          .toMap
        if (missing.isEmpty) {
          calc.allOf(validations)
        } else {
          calc.allOf(validations :+ calc.invalid(MissingProperties(missing)))
        }
      }
      case _ => calc.invalid(TypeMismatch(schema))
    }
  }
}

case class RootSchemaEvaluator(evaluator: Evaluator, defs: Map[String, Evaluator]) extends Evaluator {
  override def eval[R](value: Value)(calc: ResultCalculator[R])(implicit dereference: Dereferencer): R =
    evaluator.eval(value)(calc)(dereferenceDefs)

  private def dereferenceDefs(ref: String): Option[Evaluator] =
    defs.get(relativeize(ref))

  private def relativeize(ref: String): String = if (ref.startsWith("#/$defs/"))
    ref.substring("#/$defs/".length())
  else
    ref
}

case class RefEvaluator(ref: String) extends Evaluator {
  override def eval[R](value: Value)(calc: ResultCalculator[R])(implicit dereference: Dereferencer): R =
    dereference(ref)
      .map(_.eval(value)(calc))
      .getOrElse(calc.invalid(MissingRef(ref)))
}

case class AllOfEvaluator(es: Seq[Evaluator]) extends Evaluator {
  override def eval[R](value: Value)(calc: ResultCalculator[R])(implicit dereference: Dereferencer): R =
    calc.allOf(es.map(_.eval(value)(calc)))
}

case class AnyOfEvaluator(es: Seq[Evaluator]) extends Evaluator {
  override def eval[R](value: Value)(calc: ResultCalculator[R])(implicit dereference: Dereferencer): R =
    calc.anyOf(es.map(_.eval(value)(calc)))
}

case class OneOfEvaluator(es: Seq[Evaluator]) extends Evaluator {
  override def eval[R](value: Value)(calc: ResultCalculator[R])(implicit dereference: Dereferencer): R = {
    calc.oneOf(es.map(_.eval(value)(calc)))
  }
}

case class NotEvaluator(e: Evaluator) extends Evaluator {
  override def eval[R](value: Value)(calc: ResultCalculator[R])(implicit dereference: Dereferencer): R = {
    calc.not(e.eval(value)(calc))
  }
}

case class IfThenElseEvaluator(ifE: Evaluator, thenE: Evaluator, elseE: Evaluator) extends Evaluator {
  override def eval[R](value: Value)(calc: ResultCalculator[R])(implicit dereference: Dereferencer): R = {
    calc.ifThenElse(ifE.eval(value)(calc), thenE.eval(value)(calc), elseE.eval(value)(calc))
  }
}

case class EnumEvaluator(schema: Schema)(evaluator: Evaluator, values: Seq[Value]) extends Evaluator {
  override def eval[R](value: Value)(calc: ResultCalculator[R])(implicit dereference: Dereferencer): R = {
    val current     = evaluator.eval(value)(calc)
    val validResult = calc.valid(schema)
    if (current == validResult && values.contains(value)) {
      calc.valid(schema)
    } else {
      calc.invalid(NotInEnum(values))
    }
  }
}
