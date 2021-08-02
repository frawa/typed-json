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
  def apply(schema: SchemaValue): LoadedSchemasResolver = loadSchemas(schema.value, None)

  val empty = LoadedSchemasResolver(None)

  private def loadSchemas(value: Value, base: Option[URI]): LoadedSchemasResolver = {
    value match {
      case ObjectValue(properties) =>
        properties
          .foldLeft(empty) { case (loaded, (property, propertyValue)) =>
            (property, propertyValue) match {
              case ("$id", StringValue(id)) => loaded.add(URI.create(id), SchemaValue(value))
              case ("$anchor", StringValue(anchor)) =>
                val fragment = URI.create("#" + anchor)
                val uri      = base.map(_.resolve(fragment)).getOrElse(fragment)
                loaded.add(uri, SchemaValue(value))
              case _ => loaded.addAll(loadSchemas(propertyValue, loaded.base.orElse(base)))
            }
          }
      case _ => empty
    }
  }

}

case class LoadedSchemasResolver(override val base: Option[URI], schemas: Map[URI, SchemaValue] = Map.empty)
    extends SchemaResolver {
  def add(uri: URI, schema: SchemaValue): LoadedSchemasResolver = {
    LoadedSchemasResolver(Some(uri), schemas + ((uri, schema)))
  }
  def addAll(other: LoadedSchemasResolver): LoadedSchemasResolver = LoadedSchemasResolver(
    base,
    schemas.concat(other.schemas.toIterable)
  )
  override def resolve(uri: URI): Option[SchemaValue] = schemas.get(uri)
}
