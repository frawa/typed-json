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

object LoadedSchemasResolver {
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
                loaded.add(uri1, SchemaValue(value)).withBase(uri1)
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
    dynamicSchemas: Set[URI] = Set.empty
) extends SchemaResolver {

  def add(uri: URI, schema: SchemaValue): LoadedSchemasResolver =
    this.copy(schemas = schemas + ((uri, schema)))

  def addAll(other: LoadedSchemasResolver): LoadedSchemasResolver = this.copy(
    schemas = schemas.concat(other.schemas.toIterable),
    dynamicSchemas = dynamicSchemas.concat(other.dynamicSchemas.toIterable)
  )

  def addDynamic(uri: URI, schema: SchemaValue): LoadedSchemasResolver =
    add(uri, schema).copy(dynamicSchemas = dynamicSchemas + uri)

  private def withBase(uri: URI): LoadedSchemasResolver = this.copy(base = Some(uri))

  override def resolve(uri: URI): Option[Resolution] = schemas.get(uri).map((_, withBase(uri)))

  override def resolveDynamic(uri: URI, scope: DynamicScope): Option[Resolution] = {
    if (uri.getFragment != null) {
      val fragment = uri.getFragment()
      val selfCandidate = scope.candidates.lastOption
        .map(DynamicScope.withFragment(_, fragment))
        .filter(dynamicSchemas.contains(_))
        .orElse(Some(uri))
        .filter(dynamicSchemas.contains(_))
      val dynamicly = selfCandidate
        .filter(dynamicSchemas.contains(_))
        .flatMap { self =>
          scope.candidates
            .map(DynamicScope.withFragment(_, fragment))
            .filter(dynamicSchemas.contains(_))
            .flatMap { candidate =>
              schemas
                .get(candidate)
                .map((_, withBase(candidate)))
            }
            .headOption
        }
        .orElse(resolve(uri))
      return dynamicly
    } else {
      None
    }
  }
}
