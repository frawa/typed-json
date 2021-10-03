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

case object LoadedSchemasResolver {
  val empty = LoadedSchemasResolver(None)
  def apply(schema: SchemaValue): LoadedSchemasResolver = {
    val firstId = (Pointer.empty / "$id")(schema.value)
      .flatMap {
        case StringValue(id) => Some(URI.create(id))
        case _               => None
      }
      .getOrElse(URI.create(""))
    val first = empty.add(firstId, schema).withBase(firstId)
    loadSchemas(schema.value, first)
  }
  def apply(schemas: Seq[SchemaValue]): LoadedSchemasResolver = schemas.foldLeft(empty) { case (resolver, schema) =>
    resolver.addAll(apply(schema))
  }

  private def loadSchemas(value: Value, loaded: LoadedSchemasResolver): LoadedSchemasResolver = {
    value match {
      case ObjectValue(properties) =>
        properties
          .foldLeft(loaded) { case (loaded, (property, propertyValue)) =>
            (property, propertyValue) match {
              case ("$id", StringValue(id)) =>
                val uri  = URI.create(id).normalize()
                val uri1 = new URI(uri.getScheme, uri.getSchemeSpecificPart, null)
                loaded.add(uri1, SchemaValue(value))
              case ("$anchor", StringValue(anchor)) =>
                val fragment = URI.create("#" + anchor)
                val uri = loaded.base
                  .map(_.resolve(fragment))
                  .getOrElse(fragment)
                loaded.add(uri, SchemaValue(value))
              case ("$dynamicAnchor", StringValue(anchor)) =>
                val fragment = URI.create("#" + anchor)
                val uri = loaded.base
                  .map(_.resolve(fragment))
                  .getOrElse(fragment)
                loaded.addDynamic(uri, SchemaValue(value))
              case _ => loaded.addAll(loadSchemas(propertyValue, loaded))
            }
          }
      case _ => empty
    }
  }

}

case class LoadedSchemasResolver(
    override val base: Option[URI],
    schemas: Map[URI, SchemaValue] = Map.empty,
    dynamicSchemas: Map[URI, SchemaValue] = Map.empty,
    override val scope: Seq[URI] = Seq.empty
) extends SchemaResolver {

  def add(uri: URI, schema: SchemaValue): LoadedSchemasResolver =
    this.copy(base = Some(uri), schemas = schemas + ((uri, schema)))

  def addAll(other: LoadedSchemasResolver): LoadedSchemasResolver = this.copy(
    schemas = schemas.concat(other.schemas.toIterable),
    dynamicSchemas = dynamicSchemas.concat(other.dynamicSchemas.toIterable)
  )

  def addDynamic(uri: URI, schema: SchemaValue): LoadedSchemasResolver =
    this.copy(dynamicSchemas = dynamicSchemas + ((uri, schema)))

  private def withBase(uri: URI): LoadedSchemasResolver  = this.copy(base = Some(uri))
  private def withScope(uri: URI): LoadedSchemasResolver = this.copy(scope = scope :+ uri)
  override def resolve(uri: URI): Option[Resolution]     = schemas.get(uri).map((_, withBase(uri).withScope(uri)))
  override def resolveDynamic(uri: URI): Option[Resolution] = {
    if (uri.getFragment != null && dynamicSchemas.contains(uri)) {
      val fragment = uri.getFragment()
      scope
        .flatMap { u =>
          val candidate = u.resolve("#" + fragment)
          dynamicSchemas.get(candidate)
        }
        .headOption
        .orElse(dynamicSchemas.get(uri))
        .map((_, this))
    } else {
      None
    }
  }
}
