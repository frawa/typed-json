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

object SchemaResolver {
  type Resolution = (SchemaValue, SchemaResolver)
}
trait SchemaResolver {
  type Resolution = SchemaResolver.Resolution

  // TODO remove Option?
  val base: Option[URI] = None

  protected def resolve(uri: URI): Option[Resolution] = None
  protected def isDynamic(uri: URI): Boolean          = false

  def resolveDynamicRef(ref: String, scope: DynamicScope): Option[Resolution] = {
    resolveDynamicRef(absolute(ref), scope)
  }

  private def resolveDynamicRef(uri: URI, scope: DynamicScope): Option[Resolution] = {
    val resolved = resolveRef(uri)

    val fragment = uri.getFragment()
    val dynamic = isDynamic(uri) ||
      scope.candidates.lastOption.map(DynamicScope.withFragment(_, fragment)).exists(isDynamic((_)))
    if (dynamic && fragment != null) {
      scope.candidates
        .map(DynamicScope.withFragment(_, fragment))
        .filter(isDynamic(_))
        .headOption
        .flatMap(resolve(_))
        .orElse(resolved)
    } else {
      resolved
    }
  }

  def absolute(ref: String): URI = {
    val uri = URI.create(ref)
    withoutEmptyFragment(
      Some(uri)
        .filter(_.isAbsolute())
        .orElse(base.map(_.resolve(uri)))
        .getOrElse(uri)
    )
  }

  private def withoutEmptyFragment(uri: URI): URI = {
    val fragment = uri.getFragment()
    if (fragment != null && fragment.isEmpty()) {
      DynamicScope.withoutFragement(uri)
    } else {
      uri
    }
  }

  def resolveRef(ref: String): Option[Resolution] = {
    resolveRef(absolute(ref))
  }

  private def resolveRef(uri: URI): Option[Resolution] = {
    if (uri.getFragment != null && uri.getFragment.startsWith("/")) {
      val pointer = Pointer.parse(uri.getFragment())
      resolve(DynamicScope.withoutFragement(uri))
        .flatMap(resolvePointer(_, pointer))
    } else {
      resolve(uri)
    }
  }

  private def resolvePointer(resolution: Resolution, pointer: Pointer): Option[Resolution] = {
    val (schema, scope) = resolution
    pointer(schema.value).map(SchemaValue(_)).map((_, scope))
  }
}
