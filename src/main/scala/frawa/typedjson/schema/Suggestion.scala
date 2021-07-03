package frawa.typedjson.schema

import frawa.typedjson.parser.Value
import shapeless.the
import frawa.typedjson.parser.ObjectValue
import frawa.typedjson.parser.NullValue
import frawa.typedjson.parser.ArrayValue
import frawa.typedjson.parser.BoolValue
import frawa.typedjson.parser.StringValue
import frawa.typedjson.parser.NumberValue

trait Suggestion {
  type Dereferencer = String => Option[Quickfix]
  def suggestions(value: Value)(implicit dereference: Dereferencer): SuggestionResult
}

object Suggestion {
  def suggestions(schema: Schema)(value: Value): SuggestionResult = {
    implicit val dereference: String => Option[Evaluator[SuggestionResult]] = ref => None
    Evaluator(schema)(SuggestionResultFactory).eval(value)
  }
}

case class SuggestionResult(suggestions: Seq[Value])

object SuggestionResultFactory extends EvalResultFactory[SuggestionResult] {
  def valid(schema: Schema): SuggestionResult = SuggestionResult(Seq(DefaultValues(schema)))

  override def invalid(observation: Observation): SuggestionResult = {
    observation match {
      case MissingProperties(properties) =>
        SuggestionResult(Seq(ObjectValue(properties.map { case (key, schema) =>
          key -> DefaultValues(schema)
        }.toMap)))
      // TODO report errored type as case class?
      case TypeMismatch(t) =>
        t match {
          case "null"    => SuggestionResult(Seq(DefaultValues(NullSchema)))
          case "number"  => SuggestionResult(Seq(DefaultValues(NumberSchema)))
          case "string"  => SuggestionResult(Seq(DefaultValues(StringSchema)))
          case "boolean" => SuggestionResult(Seq(DefaultValues(BooleanSchema)))
          case _         => SuggestionResult(Seq())
        }
      case NotInEnum(values) => SuggestionResult(values)
      case _                 => SuggestionResult(Seq())
    }
  }

  private def objectAt(segments: Seq[Token], value: Value): Value = {
    segments match {
      case FieldToken(key) :: as => ObjectValue(Map(key -> objectAt(as, value)))
      case ArrayIndexToken(index) :: as =>
        ArrayValue(
          if (index > 0) Seq.fill(index - 1)(NullValue) :+ value
          else Seq(value)
        )
      case Nil => value
    }
  }

  def prefix(prefix: Pointer, result: SuggestionResult): SuggestionResult = {
    val values = result.suggestions.map(objectAt(prefix.segments, _))
    SuggestionResult(values)
  }

  def allOf(results: Seq[SuggestionResult]): SuggestionResult = {
    val suggestions = results.flatMap(_.suggestions)
    SuggestionResult(suggestions)
  }

  // def allOf(results: Seq[SuggestionResult]): SuggestionResult = {
  //   val properties = results.flatMap(_.suggestions).flatMap { case ObjectValue(properties) => properties }.toMap
  //   println("FW", results, properties)
  //   if (properties.isEmpty) {
  //     init()
  //   } else {
  //     SuggestionResult(Seq(ObjectValue(properties)))
  //   }
  // }

  def anyOf(results: Seq[SuggestionResult]): SuggestionResult = allOf(results)
  def oneOf(results: Seq[SuggestionResult]): SuggestionResult = allOf(results)
  def not(result: SuggestionResult): SuggestionResult         = SuggestionResult(Seq())
  def ifThenElse(
      ifResult: SuggestionResult,
      thenResult: SuggestionResult,
      elseResult: SuggestionResult
  ): SuggestionResult =
    allOf(Seq(ifResult, thenResult, elseResult))
}

object DefaultValues {
  // TODO use evaluators instead
  def apply(schema: Schema): Value = schema match {
    case NullSchema                                      => NullValue
    case TrueSchema                                      => BoolValue(true)
    case FalseSchema                                     => BoolValue(false)
    case BooleanSchema                                   => BoolValue(true)
    case StringSchema                                    => StringValue("")
    case NumberSchema                                    => NumberValue(0)
    case ArraySchema(items)                              => ArrayValue(Seq())
    case ObjectSchema(properties)                        => ObjectValue(Map())
    case RootSchema(_, schema, _)                        => apply(schema)
    case RefSchema(ref)                                  => NullValue // TODO pass dereferenced schema
    case SchemaWithApplicators(schema, _)                => apply(schema)
    case SchemaWithValidators(schema, Validators(enum1)) => enum1.flatMap(_.headOption).getOrElse(apply(schema))
  }
}
