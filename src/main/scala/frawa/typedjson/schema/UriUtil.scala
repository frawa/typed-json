package frawa.typedjson.schema

import java.net.URI

object UriUtil {

  def withoutFragement(uri: URI): URI = new URI(uri.getScheme(), uri.getSchemeSpecificPart(), null)

  def withFragment(uri: URI, pointer: Pointer): URI =
    new URI(uri.getScheme(), uri.getSchemeSpecificPart(), pointer.toString)

  def withFragment(uri: URI, fragment: String): URI =
    new URI(uri.getScheme(), uri.getSchemeSpecificPart(), fragment)

}
