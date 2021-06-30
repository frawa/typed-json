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

  override def valid(): QuickfixResult = QuickfixResultEmpty

  override def invalid(reason: Reason): QuickfixResult = reason match {
    case MissingProperty(key) => QuickfixResultFixes(Seq(AddProperty(Pointer.empty, key)))
  }

  override def isValid(a: QuickfixResult): Boolean = a == QuickfixResultEmpty

  override def prefix(pointer: Pointer, a: QuickfixResult): QuickfixResult = {
    a match {
      case QuickfixResultFixes(fixes) => QuickfixResultFixes(fixes.map(_.prefix(pointer)))
      case _                          => a
    }
  }

  override def and(a: QuickfixResult, b: QuickfixResult): QuickfixResult = {
    (a, b) match {
      case (QuickfixResultFixes(fa), QuickfixResultFixes(fb)) => QuickfixResultFixes(fa ++ fb)
      case (QuickfixResultFixes(_), _)                        => a
      case (_, QuickfixResultFixes(_))                        => b
      case _                                                  => QuickfixResultEmpty
    }
  }

  def or(a: QuickfixResult, b: QuickfixResult): QuickfixResult = {
    QuickfixResultEmpty
  }
}
