package frawa.typedjson.schema

import frawa.typedjson.parser.Value

trait Quickfix {
  type Dereferencer = String => Option[Quickfix]
  def fixes(value: Value)(implicit dereference: Dereferencer): QuickfixResult
}

object Quickfix {
  def apply(schema: Schema): Quickfix = ???

  def fixes(quickfix: Quickfix)(value: Value): QuickfixResult = {
    implicit val dereference: String => Option[Quickfix] = ref => None
    quickfix.fixes(value)
  }
}

trait QuickfixResult {
  val fixes: Seq[QuickfixItem]
  def and(other: QuickfixResult): QuickfixResult = QuickfixResult.and(this, other)
  def or(other: QuickfixResult): QuickfixResult  = QuickfixResult.or(this, other)
  def prefix(pointer: Pointer)                   = QuickfixResult.prefix(this, pointer)
}

trait QuickfixItem
case class AddProperty(pointer: Pointer, key: String) extends QuickfixItem

object QuickfixResult {
  def and(a: QuickfixResult, b: QuickfixResult): QuickfixResult = ???

  def or(a: QuickfixResult, b: QuickfixResult): QuickfixResult = ???

  def prefix(a: QuickfixResult, prefix: Pointer): QuickfixResult = ???
}
