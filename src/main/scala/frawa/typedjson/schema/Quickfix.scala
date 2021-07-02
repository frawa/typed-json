package frawa.typedjson.schema

import frawa.typedjson.parser.Value

trait Quickfix {
  type Dereferencer = String => Option[Quickfix]
  def fixes(value: Value)(implicit dereference: Dereferencer): QuickfixResult
}

object Quickfix {
  def fixes(schema: Schema)(value: Value): QuickfixResult = {
    implicit val dereference: String => Option[Evaluator[QuickfixResult]] = ref => None
    Evaluator(schema)(QuickfixResultFactory).eval(value)
  }
}

trait QuickfixItem {
  def prefix(pointer: Pointer): QuickfixItem
}
case class AddProperty(pointer: Pointer, key: String) extends QuickfixItem {
  override def prefix(p: Pointer): QuickfixItem = AddProperty(p / pointer, key)
}

trait QuickfixResult
case object QuickfixResultEmpty                                          extends QuickfixResult
case class QuickfixResultFixes(fixes: Seq[QuickfixItem])                 extends QuickfixResult
case class QuickfixResultAlternatives(alternatives: Seq[QuickfixResult]) extends QuickfixResult

object QuickfixResultFactory extends EvalResultFactory[QuickfixResult] {
  def init(): QuickfixResult = QuickfixResultEmpty

  override def create(observation: Observation): QuickfixResult = {
    observation match {
      case MissingProperty(key) => QuickfixResultFixes(Seq(AddProperty(Pointer.empty, key)))
      case _                    => QuickfixResultEmpty
    }
  }

  def prefix(prefix: Pointer, result: QuickfixResult): QuickfixResult = {
    result match {
      case QuickfixResultFixes(fixes) => QuickfixResultFixes(fixes.map(_.prefix(prefix)))
      case _                          => QuickfixResultEmpty
    }
  }

  def allOf(results: Seq[QuickfixResult]): QuickfixResult = {
    val fixes = results.flatMap {
      case QuickfixResultFixes(fixes) => fixes
      case _                          => Seq()
    }
    if (fixes.isEmpty) {
      QuickfixResultEmpty
    } else {
      QuickfixResultFixes(fixes)
    }
  }

  def anyOf(results: Seq[QuickfixResult]): QuickfixResult = QuickfixResultEmpty
  def oneOf(results: Seq[QuickfixResult]): QuickfixResult = QuickfixResultEmpty
  def not(result: QuickfixResult): QuickfixResult         = QuickfixResultEmpty
}
