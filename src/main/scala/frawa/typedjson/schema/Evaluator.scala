package frawa.typedjson.schema

import frawa.typedjson.parser.Value
import frawa.typedjson.parser.NullValue
import frawa.typedjson.parser.BoolValue
import frawa.typedjson.parser.StringValue
import frawa.typedjson.parser.NumberValue
import frawa.typedjson.parser.ArrayValue
import frawa.typedjson.parser.ObjectValue

trait Observation
case class TypeMismatch(expected: String)                     extends Observation
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
trait Evaluator[R] {
  type Dereferencer = String => Option[Evaluator[R]]
  def eval(value: Value)(implicit dereference: Dereferencer): R
}

object Evaluator {
  def apply[R](schema: Schema)(implicit factory: EvalResultFactory[R]): Evaluator[R] = {
    implicit val schema1 = schema
    schema match {
      case NullSchema                        => NullEvaluator()
      case TrueSchema                        => AlwaysEvaluator[R](true)
      case FalseSchema                       => AlwaysEvaluator[R](false)
      case BooleanSchema                     => BooleanEvaluator[R]()
      case StringSchema                      => StringEvaluator[R]()
      case NumberSchema                      => NumberEvaluator[R]()
      case UnionSchema(schemas: Seq[Schema]) => OneOfEvaluator(schemas.map(Evaluator(_)))
      case ArraySchema(items)                => ArrayEvaluator[R](Evaluator[R](items))
      case ObjectSchema(properties) =>
        ObjectEvaluator[R](properties)
      case RootSchema(_, schema, defs) =>
        RootSchemaEvaluator[R](Evaluator[R](schema), defs.view.mapValues(Evaluator[R](_)).toMap)
      case RefSchema(ref) => RefEvaluator[R](ref)
      case SchemaWithApplicators(schema, applicators) =>
        AllOfEvaluator(
          Seq(
            Evaluator(schema),
            applicators.allOf.map(_.map(Evaluator(_))).map(AllOfEvaluator(_)).getOrElse(AlwaysEvaluator(true)),
            applicators.anyOf.map(_.map(Evaluator(_))).map(AnyOfEvaluator(_)).getOrElse(AlwaysEvaluator(true)),
            applicators.oneOf.map(_.map(Evaluator(_))).map(OneOfEvaluator(_)).getOrElse(AlwaysEvaluator(true)),
            applicators.notOp.map(schema => NotEvaluator(Evaluator(schema))).getOrElse(AlwaysEvaluator(true)),
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
      case SchemaWithValidators(schema, validators) =>
        AllOfEvaluator(
          Seq(
            validators.`enum`.map(EnumEvaluator(Evaluator(schema), _)).getOrElse(AlwaysEvaluator(true))
          )
        )
    }
  }
}

case class NullEvaluator[R]()(implicit schema: Schema, factory: EvalResultFactory[R]) extends Evaluator[R] {
  override def eval(value: Value)(implicit dereference: Dereferencer): R = value match {
    case NullValue => factory.valid(schema)
    case _         => factory.invalid(TypeMismatch("null"))
  }
}

case class AlwaysEvaluator[R](valid: Boolean)(implicit schema: Schema, factory: EvalResultFactory[R])
    extends Evaluator[R] {
  override def eval(value: Value)(implicit dereference: Dereferencer): R = if (valid) {
    factory.valid(schema)
  } else {
    factory.invalid(FalseSchemaReason())
  }
}

case class BooleanEvaluator[R]()(implicit schema: Schema, factory: EvalResultFactory[R]) extends Evaluator[R] {
  override def eval(value: Value)(implicit dereference: Dereferencer): R = value match {
    case BoolValue(value) => if (value) factory.valid(schema) else factory.invalid(FalseSchemaReason())
    case _                => factory.invalid(TypeMismatch("boolean"))
  }
}

case class StringEvaluator[R]()(implicit schema: Schema, factory: EvalResultFactory[R]) extends Evaluator[R] {
  override def eval(value: Value)(implicit dereference: Dereferencer): R = value match {
    case StringValue(_) => factory.valid(schema)
    case _              => factory.invalid(TypeMismatch("string"))
  }
}

case class NumberEvaluator[R]()(implicit schema: Schema, factory: EvalResultFactory[R]) extends Evaluator[R] {
  override def eval(value: Value)(implicit dereference: Dereferencer): R = value match {
    case NumberValue(_) => factory.valid(schema)
    case _              => factory.invalid(TypeMismatch("number"))
  }
}

case class ArrayEvaluator[R](itemsEvaluator: Evaluator[R])(implicit schema: Schema, factory: EvalResultFactory[R])
    extends Evaluator[R] {
  override def eval(value: Value)(implicit dereference: Dereferencer): R = {
    value match {
      case ArrayValue(items) =>
        factory.allOf(
          items.zipWithIndex
            .map { case (item, index) =>
              lazy val prefix = Pointer.empty / index
              factory.prefix(prefix, itemsEvaluator.eval(item))
            }
        )
      case _ => factory.invalid(TypeMismatch("array"))
    }
  }
}

case class ObjectEvaluator[R](schemaByProperty: Map[String, Schema])(implicit
    schema: Schema,
    factory: EvalResultFactory[R]
) extends Evaluator[R] {
  override def eval(value: Value)(implicit dereference: Dereferencer): R = {
    value match {
      case ObjectValue(properties) => {
        val validations = properties.map { case (key1, value1) =>
          lazy val prefix = Pointer.empty / key1
          schemaByProperty
            .get(key1)
            .map(Evaluator(_))
            .map(_.eval(value1))
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
      case _ => factory.invalid(TypeMismatch("object"))
    }
  }
}

case class RootSchemaEvaluator[R](evaluator: Evaluator[R], defs: Map[String, Evaluator[R]])(implicit
    schema: Schema,
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

case class RefEvaluator[R](ref: String)(implicit schema: Schema, factory: EvalResultFactory[R]) extends Evaluator[R] {
  override def eval(value: Value)(implicit dereference: Dereferencer): R =
    dereference(ref)
      .map(_.eval(value))
      .getOrElse(factory.invalid(MissingRef(ref)))
}

case class AllOfEvaluator[R](es: Seq[Evaluator[R]])(implicit schema: Schema, factory: EvalResultFactory[R])
    extends Evaluator[R] {
  override def eval(value: Value)(implicit dereference: Dereferencer): R =
    factory.allOf(es.map(_.eval(value)))
}

case class AnyOfEvaluator[R](es: Seq[Evaluator[R]])(implicit schema: Schema, factory: EvalResultFactory[R])
    extends Evaluator[R] {
  override def eval(value: Value)(implicit dereference: Dereferencer): R =
    factory.anyOf(es.map(_.eval(value)))
}

case class OneOfEvaluator[R](es: Seq[Evaluator[R]])(implicit schema: Schema, factory: EvalResultFactory[R])
    extends Evaluator[R] {
  override def eval(value: Value)(implicit dereference: Dereferencer): R = {
    factory.oneOf(es.map(_.eval(value)))
    // if (es.isEmpty) {
    //   return factory.init())
    // }
    // val results    = es.map(_.eval(value))
    // val countValid = results.count(result => factory.isEmpty(result.result))
    // if (countValid == 1) {
    //   factory.init()
    // } else {
    //   factory.create(NotOneOf(countValid))
    // })
  }
}

case class NotEvaluator[R](e: Evaluator[R])(implicit schema: Schema, factory: EvalResultFactory[R])
    extends Evaluator[R] {
  override def eval(value: Value)(implicit dereference: Dereferencer): R = {
    factory.not(e.eval(value))
    // if (factory.isEmpty(result.result)) {
    //   factory.create(NotInvalid())
    // } else {
    //   factory.init()
    // }
  }
}

case class IfThenElseEvaluator[R](ifE: Evaluator[R], thenE: Evaluator[R], elseE: Evaluator[R])(implicit
    factory: EvalResultFactory[R]
) extends Evaluator[R] {
  override def eval(value: Value)(implicit dereference: Dereferencer): R = {
    factory.ifThenElse(ifE.eval(value), thenE.eval(value), elseE.eval(value))
  }
}

case class EnumEvaluator[R](evaluator: Evaluator[R], values: Seq[Value])(implicit
    schema: Schema,
    factory: EvalResultFactory[R]
) extends Evaluator[R] {
  override def eval(value: Value)(implicit dereference: Dereferencer): R = {
    if (evaluator.eval(value) == factory.valid(schema) && values.contains(value)) {
      factory.valid(schema)
    } else {
      factory.invalid(NotInEnum(values))
    }
  }
}

object Helper {

  def mapNonEmpty[T, S](as: Seq[T])(f: T => S): Option[Seq[S]] =
    if (as.isEmpty) {
      None
    } else {
      Some(as.map(f))
    }

  def debugTraceValue[T](title: String): T => T = { v =>
    println(title, v)
    v
  }
}
