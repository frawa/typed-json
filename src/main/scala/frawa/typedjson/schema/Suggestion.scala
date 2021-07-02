package frawa.typedjson.schema

import frawa.typedjson.parser.Value
import shapeless.the
import frawa.typedjson.parser.ObjectValue
import frawa.typedjson.parser.NullValue
import frawa.typedjson.parser.ArrayValue

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
  def init(): SuggestionResult = SuggestionResult(Seq())

  override def create(observation: Observation): SuggestionResult = {
    observation match {
      case MissingProperty(key) => SuggestionResult(Seq(ObjectValue(Map(key -> NullValue))))
      case _                    => init()
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

  def anyOf(results: Seq[SuggestionResult]): SuggestionResult = allOf(results)

  def oneOf(results: Seq[SuggestionResult]): SuggestionResult = anyOf(results)
  def not(result: SuggestionResult): SuggestionResult         = init()
  def ifThenElse(
      ifResult: SuggestionResult,
      thenResult: SuggestionResult,
      elseResult: SuggestionResult
  ): SuggestionResult =
    allOf(Seq(ifResult, thenResult, elseResult))
}
