package frawa.typedjson.schema

import frawa.typedjson.parser.Value
import frawa.typedjson.parser.StringValue
import java.net.URI

case class DynamicScope(uris: Seq[URI]) {
  import DynamicScope._
  import UriUtil._

  def candidates: Seq[URI] = uris
    .map(withoutFragement)
    .distinct

  def push(segment: String): DynamicScope = {
    val uri = uris.lastOption.getOrElse(URI.create(""))
    val pointer = uris.lastOption
      .map(_.getFragment())
      .filter(_ != null)
      .map(Pointer.parse(_))
      .getOrElse(Pointer.empty)
    val pushPointer = pointer / segment
    val next        = withFragment(uri, pushPointer)
    DynamicScope(uris :+ next)
  }

  def push(next: URI): DynamicScope = {
    DynamicScope(uris :+ next)
  }
}

object DynamicScope {
  def empty: DynamicScope = DynamicScope(Seq())
}
