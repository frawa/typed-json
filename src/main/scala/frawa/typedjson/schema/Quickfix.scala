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
case class AddProperties(pointer: Pointer, keys: Seq[String]) extends QuickfixItem {
  override def prefix(prefix: Pointer): QuickfixItem = AddProperties(prefix / pointer, keys)
}

case class QuickfixItemGroup(items: Seq[QuickfixItem]) extends QuickfixItem {
  def prefix(pointer: Pointer): QuickfixItem = {
    QuickfixItemGroup(items.map(_.prefix(pointer)))
  }
}

trait QuickfixResult
case object QuickfixResultEmpty                          extends QuickfixResult
case class QuickfixResultFixes(fixes: Seq[QuickfixItem]) extends QuickfixResult

object QuickfixResultFactory extends EvalResultFactory[QuickfixResult] {
  def valid(schema: Schema): QuickfixResult = QuickfixResultEmpty

  override def invalid(observation: Observation): QuickfixResult = {
    observation match {
      case MissingProperties(properties) =>
        QuickfixResultFixes(Seq(AddProperties(Pointer.empty, properties.keySet.toSeq)))
      case _ => QuickfixResultEmpty
    }
  }

  def prefix(prefix: Pointer, result: QuickfixResult): QuickfixResult = {
    result match {
      case QuickfixResultFixes(fixes) => QuickfixResultFixes(fixes.map(_.prefix(prefix)))
      case _                          => QuickfixResultEmpty
    }
  }

  private def groupLeafItems(items: Seq[QuickfixItem]): Seq[QuickfixItem] = {
    val (groups, leafs) = items.partition(_.isInstanceOf[QuickfixItemGroup])
    if (leafs.isEmpty) {
      groups
    } else {
      groups :+ QuickfixItemGroup(leafs)
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

  def anyOf(results: Seq[QuickfixResult]): QuickfixResult = {
    val fixes = results.flatMap {
      case QuickfixResultFixes(fixes) => groupLeafItems(fixes)
      case _                          => Seq()
    }
    if (fixes.isEmpty) {
      QuickfixResultEmpty
    } else {
      QuickfixResultFixes(fixes)
    }
  }

  def oneOf(results: Seq[QuickfixResult]): QuickfixResult = anyOf(results)
  def not(result: QuickfixResult): QuickfixResult         = QuickfixResultEmpty
  def ifThenElse(
      ifResult: QuickfixResult,
      thenResult: QuickfixResult,
      elseResult: QuickfixResult
  ): QuickfixResult =
    allOf(Seq(ifResult, thenResult, elseResult))
}
