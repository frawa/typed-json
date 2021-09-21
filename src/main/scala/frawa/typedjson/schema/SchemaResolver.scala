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

case class DynamicScope(uris: Seq[URI])

trait SchemaResolver {
  type Resolution = (SchemaValue, SchemaResolver)

  val base: Option[URI] = None
  val scope: Seq[URI]   = Seq.empty

  protected def resolve(uri: URI): Option[Resolution]        = None
  protected def resolveDynamic(uri: URI): Option[Resolution] = None

  def resolveDynamicRef(ref: String): Option[Resolution] = {
    def uri = URI.create(ref)
    if (uri.isAbsolute()) {
      resolveDynamic(uri)
    } else {
      base
        .map(_.resolve(uri))
        .flatMap(resolveDynamic(_))
    }
  }

  def resolveRef(ref: String): Option[Resolution] = {
    def uri = URI.create(ref)
    if (uri.isAbsolute()) {
      resolve(uri)
    } else if (uri.getFragment != null && uri.getFragment.startsWith("/")) {
      val pointer = Pointer.parse(uri.getFragment())
      base
        .flatMap(resolve(_))
        .flatMap(resolvePointer(_, pointer))
    } else {
      base
        .map(_.resolve(uri))
        .flatMap(resolve(_))
    }
  }

  private def resolvePointer(resolution: Resolution, pointer: Pointer): Option[Resolution] = {
    val (schema, scope) = resolution
    pointer(schema.value).map(SchemaValue(_)).map((_, scope))
  }
}
