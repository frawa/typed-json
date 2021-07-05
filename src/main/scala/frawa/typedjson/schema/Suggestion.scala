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
    Evaluator(schema)(value)(SuggestionResultCalculator)
  }
}

case class SuggestionResult(suggestions: Seq[Value])

object SuggestionResultCalculator extends ResultCalculator[SuggestionResult] {
  def valid(schema: Schema): SuggestionResult = SuggestionResult(DefaultValues(schema))

  override def invalid(observation: Observation): SuggestionResult = {
    observation match {
      case MissingProperties(properties) =>
        val fw1 = SuggestionResult(
          properties
            .map { case (key, schema) =>
              DefaultValues(schema).map(key -> _)
            }
            .flatten
            .map { case (key, value) =>
              ObjectValue(Map(key -> value))
            }
            .toSeq
        )
        // val fw = SuggestionResult(Seq(ObjectValue(properties.flatMap { case (key, schema) =>
        //   DefaultValues(schema).map((key, _))
        // }.toMap)))
        fw1
      case TypeMismatch(schema) => SuggestionResult(DefaultValues(schema))
      case NotInEnum(values)    => SuggestionResult(values)
      case _                    => SuggestionResult(Seq())
    }
  }

  private def objectAt(segments: Seq[Token], value: Value): Value = {
    segments match {
      case FieldToken(key) :: as => ObjectValue(Map(key -> objectAt(as, value)))
      // TODO
      // case ArrayIndexToken(index) :: as =>
      //   ArrayValue(
      //     if (index > 0) Seq.fill(index - 1)(NullValue) :+ value
      //     else Seq(value)
      //   )
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
  def apply(schema: Schema): Seq[Value] = schema match {
    // case TrueSchema                       => BoolValue(true)
    // case FalseSchema                      => BoolValue(false)
    // case RefSchema(ref)                   => NullValue // TODO pass dereferenced schema?
    case NullSchema                       => Seq(NullValue)
    case BooleanSchema                    => Seq(BoolValue(true))
    case StringSchema                     => Seq(StringValue(""))
    case NumberSchema                     => Seq(NumberValue(0))
    case ArraySchema(_)                   => Seq(ArrayValue(Seq()))
    case ObjectSchema(_)                  => Seq(ObjectValue(Map()))
    case RootSchema(_, schema, _)         => apply(schema)
    case SchemaWithApplicators(schema, _) => apply(schema)
    case SchemaWithValidators(schema, Validators(enum1, const1)) =>
      val valids = enum1.toSeq.flatten ++ const1.toSeq
      if (valids.isEmpty)
        apply(schema)
      else
        valids
    case _ => Seq()
  }
}
