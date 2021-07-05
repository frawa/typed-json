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

trait EvalResultFactory[R] {
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
  def eval[R](value: Value)(factory: EvalResultFactory[R])(implicit dereference: Dereferencer): R

  def apply[R](value: Value)(factory: EvalResultFactory[R]) = {
    implicit val dereference: String => Option[Evaluator] = ref => None
    this.eval(value)(factory)
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
  override def eval[R](value: Value)(factory: EvalResultFactory[R])(implicit dereference: Dereferencer): R =
    value match {
      case NullValue => factory.valid(schema)
      case _         => factory.invalid(TypeMismatch(schema))
    }
}

case class AlwaysEvaluator(valid: Boolean) extends Evaluator {
  override def eval[R](value: Value)(factory: EvalResultFactory[R])(implicit dereference: Dereferencer): R = if (
    valid
  ) {
    factory.valid(TrueSchema)
  } else {
    factory.invalid(FalseSchemaReason())
  }
}

case class BooleanEvaluator(schema: Schema) extends Evaluator {
  override def eval[R](value: Value)(factory: EvalResultFactory[R])(implicit dereference: Dereferencer): R =
    value match {
      case BoolValue(value) => if (value) factory.valid(schema) else factory.invalid(FalseSchemaReason())
      case _                => factory.invalid(TypeMismatch(schema))
    }
}

case class StringEvaluator(schema: Schema) extends Evaluator {
  override def eval[R](value: Value)(factory: EvalResultFactory[R])(implicit dereference: Dereferencer): R =
    value match {
      case StringValue(_) => factory.valid(schema)
      case _              => factory.invalid(TypeMismatch(schema))
    }
}

case class NumberEvaluator(schema: Schema) extends Evaluator {
  override def eval[R](value: Value)(factory: EvalResultFactory[R])(implicit dereference: Dereferencer): R =
    value match {
      case NumberValue(_) => factory.valid(schema)
      case _              => factory.invalid(TypeMismatch(schema))
    }
}

case class ArrayEvaluator(schema: Schema)(itemsEvaluator: Evaluator) extends Evaluator {
  override def eval[R](value: Value)(factory: EvalResultFactory[R])(implicit dereference: Dereferencer): R = {
    value match {
      case ArrayValue(items) =>
        factory.allOf(
          items.zipWithIndex
            .map { case (item, index) =>
              lazy val prefix = Pointer.empty / index
              factory.prefix(prefix, itemsEvaluator.eval(item)(factory))
            }
        )
      case _ => factory.invalid(TypeMismatch(schema))
    }
  }
}

case class ObjectEvaluator(schema: Schema)(schemaByProperty: Map[String, Schema]) extends Evaluator {
  override def eval[R](value: Value)(factory: EvalResultFactory[R])(implicit dereference: Dereferencer): R = {
    value match {
      case ObjectValue(properties) => {
        val validations = properties.map { case (key1, value1) =>
          lazy val prefix = Pointer.empty / key1
          schemaByProperty
            .get(key1)
            .map(Evaluator(_))
            .map(_.eval(value1)(factory))
            .map(factory.prefix(prefix, _))
            .getOrElse(factory.invalid(UnexpectedProperty(key1)))
        }.toSeq
        val missing = schemaByProperty.view
          .filterKeys(!properties.contains(_))
          .toMap
        if (missing.isEmpty) {
          factory.allOf(validations)
        } else {
          factory.allOf(validations :+ factory.invalid(MissingProperties(missing)))
        }
      }
      case _ => factory.invalid(TypeMismatch(schema))
    }
  }
}

case class RootSchemaEvaluator(evaluator: Evaluator, defs: Map[String, Evaluator]) extends Evaluator {
  override def eval[R](value: Value)(factory: EvalResultFactory[R])(implicit dereference: Dereferencer): R =
    evaluator.eval(value)(factory)(dereferenceDefs)

  private def dereferenceDefs(ref: String): Option[Evaluator] =
    defs.get(relativeize(ref))

  private def relativeize(ref: String): String = if (ref.startsWith("#/$defs/"))
    ref.substring("#/$defs/".length())
  else
    ref
}

case class RefEvaluator(ref: String) extends Evaluator {
  override def eval[R](value: Value)(factory: EvalResultFactory[R])(implicit dereference: Dereferencer): R =
    dereference(ref)
      .map(_.eval(value)(factory))
      .getOrElse(factory.invalid(MissingRef(ref)))
}

case class AllOfEvaluator(es: Seq[Evaluator]) extends Evaluator {
  override def eval[R](value: Value)(factory: EvalResultFactory[R])(implicit dereference: Dereferencer): R =
    factory.allOf(es.map(_.eval(value)(factory)))
}

case class AnyOfEvaluator(es: Seq[Evaluator]) extends Evaluator {
  override def eval[R](value: Value)(factory: EvalResultFactory[R])(implicit dereference: Dereferencer): R =
    factory.anyOf(es.map(_.eval(value)(factory)))
}

case class OneOfEvaluator(es: Seq[Evaluator]) extends Evaluator {
  override def eval[R](value: Value)(factory: EvalResultFactory[R])(implicit dereference: Dereferencer): R = {
    factory.oneOf(es.map(_.eval(value)(factory)))
  }
}

case class NotEvaluator(e: Evaluator) extends Evaluator {
  override def eval[R](value: Value)(factory: EvalResultFactory[R])(implicit dereference: Dereferencer): R = {
    factory.not(e.eval(value)(factory))
  }
}

case class IfThenElseEvaluator(ifE: Evaluator, thenE: Evaluator, elseE: Evaluator) extends Evaluator {
  override def eval[R](value: Value)(factory: EvalResultFactory[R])(implicit dereference: Dereferencer): R = {
    factory.ifThenElse(ifE.eval(value)(factory), thenE.eval(value)(factory), elseE.eval(value)(factory))
  }
}

case class EnumEvaluator(schema: Schema)(evaluator: Evaluator, values: Seq[Value]) extends Evaluator {
  override def eval[R](value: Value)(factory: EvalResultFactory[R])(implicit dereference: Dereferencer): R = {
    val current     = evaluator.eval(value)(factory)
    val validResult = factory.valid(schema)
    if (current == validResult && values.contains(value)) {
      factory.valid(schema)
    } else {
      factory.invalid(NotInEnum(values))
    }
  }
}
