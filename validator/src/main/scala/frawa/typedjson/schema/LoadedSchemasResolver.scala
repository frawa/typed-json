/*
 * Copyright 2021 Frank Wagner
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package frawa.typedjson.schema

import frawa.typedjson.parser.ArrayValue
import frawa.typedjson.parser.ObjectValue
import frawa.typedjson.parser.StringValue
import frawa.typedjson.parser.Value
import UriUtil._

import java.net.URI

object LoadedSchemasResolver {
  type LazyResolver = URI => Option[SchemaValue]

  val empty: LoadedSchemasResolver = LoadedSchemasResolver(uri(""))

  def apply(schema: SchemaValue, lazyResolver: Option[LazyResolver]): LoadedSchemasResolver = {
    val resolver = apply(schema)
    lazyResolver.map(resolver.withLazyResolver(_)).getOrElse(resolver)
  }

  def apply(schema: SchemaValue): LoadedSchemasResolver = {
    val firstId = SchemaValue
      .id(schema)
      .map(uri(_))
      .getOrElse(uri(""))
    val first = empty.add(firstId, schema).withBase(firstId)
    loadSchemas(schema.value, first)
  }

  def apply(schemas: Seq[SchemaValue]): LoadedSchemasResolver = {
    schemas.foldLeft(empty) { case (resolver, schema) =>
      resolver.addAll(apply(schema))
    }
  }

  private def loadSchemas(value: Value, loaded: LoadedSchemasResolver): LoadedSchemasResolver = {
    value match {
      case ObjectValue(properties) =>
        val loaded1 = properties
          .get("$id")
          .flatMap(Value.asString)
          .map(loaded.absolute(_))
          .map(uri => loaded.add(uri, SchemaValue(value)).withBase(UriUtil.withoutFragement(uri)))
          .getOrElse(loaded)
        properties
          .foldLeft(loaded1) { case (loaded, (property, propertyValue)) =>
            (property, propertyValue) match {
              case ("$id", StringValue(id)) =>
                // already handled with loaded1
                loaded
              case ("$anchor", StringValue(anchor)) =>
                val uri = loaded.absolute("#" + anchor)
                loaded.add(uri, SchemaValue(value))
              case ("$dynamicAnchor", StringValue(anchor)) =>
                val uri = loaded.absolute("#" + anchor)
                loaded.addDynamic(uri, SchemaValue(value))
              case _ => loadNestedSchemaValues(property, propertyValue, loaded)
            }
          }
      case _ => loaded
    }
  }

  private val getterByKeyword: Map[String, NestedSchemaGetter] = Map(
    "not"                   -> selfSchema,
    "items"                 -> selfSchema,
    "prefixItems"           -> arraySchemas,
    "unevaluatedItems"      -> selfSchema,
    "properties"            -> objectSchemas,
    "patternProperties"     -> objectSchemas,
    "additionalProperties"  -> selfSchema,
    "unevaluatedProperties" -> selfSchema,
    "allOf"                 -> arraySchemas,
    "anyOf"                 -> arraySchemas,
    "oneOf"                 -> arraySchemas,
    "if"                    -> selfSchema,
    "then"                  -> selfSchema,
    "else"                  -> selfSchema,
    "$defs"                 -> objectSchemas,
    "propertyNames"         -> selfSchema,
    "dependentSchemas"      -> objectSchemas,
    "contains"              -> selfSchema
  )

  private type NestedSchemaGetter = Value => Seq[Value]

  private def selfSchema: NestedSchemaGetter    = v => Seq(v)
  private def arraySchemas: NestedSchemaGetter  = { case ArrayValue(vs) => vs }
  private def objectSchemas: NestedSchemaGetter = { case ObjectValue(ps) => ps.values.toSeq }

  private def loadNestedSchemaValues(
      property: String,
      value: Value,
      loaded: LoadedSchemasResolver
  ): LoadedSchemasResolver = {
    val nested = getterByKeyword.get(property).map(_(value)).getOrElse(Seq.empty)
    nested.foldLeft(loaded) { case (loaded, v) =>
      loaded.addAll(loadSchemas(v, loaded))
    }
  }

}

case class LoadedSchemasResolver(
    override val base: URI,
    schemas: Map[URI, SchemaValue] = Map.empty,
    dynamicSchemas: Set[URI] = Set.empty,
    lazyResolver: Option[LoadedSchemasResolver.LazyResolver] = None
) extends SchemaResolver {

  override def withBase(uri: URI): LoadedSchemasResolver = this.copy(base = uri)

  override protected def resolve(uri: URI): Option[Resolution] = schemas
    .get(uri)
    .map((_, withBase(uri)))
    .orElse(
      lazyResolver
        .flatMap(_.apply(uri))
        .map { schema =>
          val loaded1 = add(uri, schema).withBase(uri)
          val loaded2 = LoadedSchemasResolver.loadSchemas(schema.value, loaded1)
          (schema, loaded2)
        }
    )

  override protected def isDynamic(uri: URI): Boolean = dynamicSchemas.contains(uri)

  def withLazyResolver(lazyResolver: LoadedSchemasResolver.LazyResolver): LoadedSchemasResolver =
    this.copy(lazyResolver = Some(lazyResolver))

  private def add(uri: URI, schema: SchemaValue): LoadedSchemasResolver =
    this.copy(schemas = schemas + ((uri, schema)))

  private def addAll(other: LoadedSchemasResolver): LoadedSchemasResolver = this.copy(
    schemas = schemas.concat(other.schemas.toIterable),
    dynamicSchemas = dynamicSchemas.concat(other.dynamicSchemas.toIterable)
  )

  private def addDynamic(uri: URI, schema: SchemaValue): LoadedSchemasResolver =
    add(uri, schema).copy(dynamicSchemas = dynamicSchemas + uri)
}
