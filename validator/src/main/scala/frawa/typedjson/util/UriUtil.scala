package frawa.typedjson.util

import frawa.typedjson.schema.Pointer

import java.net.URI

object UriUtil {

  def uri(value: String): URI = {
    URI.create(escape(value))
  }

  def withoutFragement(uri: URI): URI = new URI(uri.getScheme(), uri.getSchemeSpecificPart(), null)

  def withFragment(uri: URI, pointer: Pointer): URI =
    new URI(uri.getScheme(), uri.getSchemeSpecificPart(), escape(pointer.toString))

  def withFragment(uri: URI, fragment: String): URI =
    new URI(uri.getScheme(), uri.getSchemeSpecificPart(), fragment)

  case class WithLocation[+T](uri: URI, value: T)

  private def escape(value: String): String = {
    // this is because the ScalaJS implementation of URI might fail
    value.replace("\\", "_")
  }
}
