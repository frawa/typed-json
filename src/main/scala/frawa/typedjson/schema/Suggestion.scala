package frawa.typedjson.schema

import frawa.typedjson.parser.Value
import shapeless.the

trait Suggestion {
  type Dereferencer = String => Option[Quickfix]
  def suggestions(value: Value)(implicit dereference: Dereferencer): SuggestionResult
}

object Suggestion {
  def suggestions(schema: Schema)(value: Value, at: Pointer): SuggestionResult = {
    implicit val dereference: String => Option[Evaluator[SuggestionResult]] = ref => None
    Evaluator(schema)(SuggestionResultFactory(at)).eval(value)
  }
}

case class SuggestionResult(suggestions: Seq[String])

case class SuggestionResultFactory(val at: Pointer) extends EvalResultFactory[SuggestionResult] {
  def init(): SuggestionResult = SuggestionResult(Seq())

  override def create(observation: Observation): SuggestionResult = {
    observation match {
      case MissingProperty(key) => SuggestionResult(Seq(key))
      case _                    => init()
    }
  }

  def prefix(prefix: Pointer, result: SuggestionResult): SuggestionResult = result

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
