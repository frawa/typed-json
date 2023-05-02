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

package frawa.typedjson.keywords

import frawa.typedjson.parser.Value
import frawa.typedjson.parser.Value.*
import frawa.typedjson.util.UriUtil
import frawa.typedjson.util.UriUtil.*

import java.net.URI
import frawa.typedjson.util.Debug

object LoadedSchemasResolver:
  type LazyResolver = URI => Option[RootSchemaValue]

  val empty: LoadedSchemasResolver = LoadedSchemasResolver(uri(""))

  def apply(schema: SchemaValue, lazyResolver: Option[LazyResolver]): LoadedSchemasResolver =
    val resolver = apply(schema)
    lazyResolver.map(resolver.withLazyResolver).getOrElse(resolver)

  def apply(schema: SchemaValue): LoadedSchemasResolver =
    val firstId = SchemaValue
      .id(schema)
      .map(uri)
      .getOrElse(uri(""))
    val first = empty.add(firstId, schema).withBase(firstId)
    loadSchemas(schema.value, first)

  def apply(schemas: Seq[SchemaValue]): LoadedSchemasResolver =
    schemas.foldLeft(empty) { case (resolver, schema) =>
      resolver.addAll(apply(schema))
    }

  private def loadSchemas(value: Value, loaded0: LoadedSchemasResolver): LoadedSchemasResolver =
    value match
      case ObjectValue(properties) =>
        val loaded1 = properties
          .get("$id")
          .flatMap(Value.asString)
          .map(loaded0.absolute)
          .map(uri => loaded0.add(uri, SchemaValue(value)).withBase(UriUtil.withoutFragement(uri)))
          .getOrElse(loaded0)
        properties
          .foldLeft(loaded1) { case (loaded, (property, propertyValue)) =>
            (property, propertyValue) match
              case ("$id", StringValue(_)) =>
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
      case _ => loaded0

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

  private def selfSchema: NestedSchemaGetter = v => Seq(v)
  private def arraySchemas: NestedSchemaGetter =
    case ArrayValue(vs) => vs
    case _              => Seq.empty
  private def objectSchemas: NestedSchemaGetter =
    case ObjectValue(ps) => ps.values.toSeq
    case _               => Seq.empty

  private def loadNestedSchemaValues(
      property: String,
      value: Value,
      loaded: LoadedSchemasResolver
  ): LoadedSchemasResolver =
    val nested = getterByKeyword.get(property).map(_(value)).getOrElse(Seq.empty)
    nested.foldLeft(loaded) { case (loaded, v) =>
      loaded.addAll(loadSchemas(v, loaded))
    }

case class LoadedSchemasResolver(
    override val base: URI,
    schemas: Map[URI, SchemaValue] = Map.empty,
    dynamicSchemas: Set[URI] = Set.empty,
    lazyResolver: Option[LoadedSchemasResolver.LazyResolver] = None
) extends SchemaResolver:

  override def withBase(uri: URI): LoadedSchemasResolver =
    this.copy(base = uri)

  override protected def resolve(uri: URI): Option[SchemaResolution] = schemas
    .get(uri)
    .map(SchemaResolution(_, this.withBase(uri)))
    .orElse(
      lazyResolver
        .flatMap(_(uri))
        .map { schema =>
          val loaded1 = this.add(uri, schema).withBase(uri)
          val loaded2 = LoadedSchemasResolver.loadSchemas(schema.value, loaded1)
          if loaded1.base != loaded2.base then {
            // TODO how to handle?
            // println(s"FW alias ${loaded1.base} ${loaded2.base}")
          }
          SchemaResolution(schema, loaded2)
        }
    )

  override protected def isDynamic(uri: URI): Boolean = dynamicSchemas.contains(uri)

  def withLazyResolver(lazyResolver: LoadedSchemasResolver.LazyResolver): LoadedSchemasResolver =
    this.copy(lazyResolver = Some(lazyResolver))

  private def add(uri: URI, schema: SchemaValue): LoadedSchemasResolver =
    // TODO how come?
    // if schemas.contains(uri) then {
    //   println(s"FW exists ${uri}")
    // }
    this.copy(schemas = schemas + ((uri, schema)))

  private def addAll(other: LoadedSchemasResolver): LoadedSchemasResolver =
    this.copy(
      schemas = schemas.concat(other.schemas.toSeq),
      dynamicSchemas = dynamicSchemas.concat(other.dynamicSchemas.toSeq)
    )

  private def addDynamic(uri: URI, schema: SchemaValue): LoadedSchemasResolver =
    add(uri, schema).copy(dynamicSchemas = dynamicSchemas + uri)
