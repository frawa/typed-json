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
  val base: Option[URI]                      = None
  def resolve(uri: URI): Option[SchemaValue] = None

  def resolveRef(ref: String): Option[SchemaValue] = {
    def uri = URI.create(ref)
    if (uri.isAbsolute()) {
      resolve(uri)
    } else if (uri.getFragment.startsWith("/")) {
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

  private def resolvePointer(schema: SchemaValue, pointer: Pointer): Option[SchemaValue] =
    pointer(schema.value).map(SchemaValue(_))
}
