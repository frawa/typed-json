package frawa.typedjson.schema

import frawa.typedjson.parser.Value
import frawa.typedjson.parser.ZioParser
import frawa.typedjson.parser.Parser
import frawa.typedjson.parser.NumberValue
import frawa.typedjson.parser.BoolValue
import frawa.typedjson.parser.NullValue
import frawa.typedjson.parser.StringValue
import frawa.typedjson.parser.ArrayValue
import frawa.typedjson.parser.ObjectValue
import scala.reflect.internal.Reporter
import java.net.URI
import scala.reflect.ClassTag

trait SchemaResolver {
  type Resolution = (SchemaValue, SchemaResolver)

  // TODO remove Option?
  val base: Option[URI] = None

  protected def resolve(uri: URI): Option[Resolution]                             = None
  protected def resolveDynamic(uri: URI, scope: DynamicScope): Option[Resolution] = None

  def resolveDynamicRef(ref: String, scope: DynamicScope): Option[Resolution] = {
    def uri = URI.create(ref)
    if (uri.isAbsolute()) {
      resolveDynamic(uri, scope)
    } else {
      base
        .map(_.resolve(uri))
        .flatMap(resolveDynamic(_, scope))
    }
  }

  def resolveRef(ref: String): Option[Resolution] = {
    resolveRef(URI.create(ref))
  }

  def resolveRef(uri: URI): Option[Resolution] = {
    // println("FW", base, uri)
    if (base.map(DynamicScope.withoutFragement(_)).contains(DynamicScope.withoutFragement(uri))) {
      println("FW BOOM", this)
      return None
    }
    if (uri.isAbsolute()) {
      if (uri.getFragment != null) {
        // TODO this is not supposed to happen?
        val uriWithoutFragment = new URI(uri.getScheme(), uri.getSchemeSpecificPart(), null)
        val pointer            = Pointer.parse(uri.getFragment())
        resolve(uriWithoutFragment)
          .flatMap(resolvePointer(_, pointer))
      } else {
        resolve(uri)
      }
    } else if (uri.getFragment != null && uri.getFragment.startsWith("/")) {
      val pointer = Pointer.parse(uri.getFragment())
      base
        .flatMap(resolve(_))
        // .orElse(resolveDynamic(uri, DynamicScope.empty))
        .flatMap(resolvePointer(_, pointer))
    } else {
      println("FW1313", uri)
      base
        .map(_.resolve(uri))
        .flatMap(resolve(_))
        .orElse(resolve(uri))
      // .orElse(resolveDynamic(uri, base.map(DynamicScope.empty.push(_)).getOrElse(DynamicScope.empty)))
    }
  }

  private def resolvePointer(resolution: Resolution, pointer: Pointer): Option[Resolution] = {
    val (schema, scope) = resolution
    pointer(schema.value).map(SchemaValue(_)).map((_, scope))
  }
}
