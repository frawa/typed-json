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

import frawa.typedjson.parser.Value._
import frawa.typedjson.util.UriUtil._
import munit.FunSuite

import java.net.URI

class SchemaResolverTest extends FunSuite:
  val fooId       = "https://example.net/foo.json"
  val fooUri: URI = uri(fooId)

  val gnuUri: URI            = uri("https://example.net/foo.json#gnu")
  val gnuSchema: SchemaValue =
    SchemaValue(
      value = ObjectValue(
        properties = Map(
          "$anchor" -> StringValue(
            value = "gnu"
          ),
          "type" -> StringValue(
            value = "string"
          )
        )
      )
    )

  val fooSchema: SchemaValue =
    SchemaValue(
      value = ObjectValue(
        properties = Map(
          "$id" -> StringValue(
            value = fooId
          ),
          "type" -> StringValue(
            value = "number"
          ),
          "$defs" -> ObjectValue(
            properties = Map(
              "gnu" -> gnuSchema.value
            )
          )
        )
      )
    )

  case object MySchemaResolver extends SchemaResolver:

    override val base: URI = fooUri

    override def resolve(uri: URI): Option[SchemaResolution] = uri match
      case `fooUri` => Some(SchemaResolution(fooSchema, this))
      case `gnuUri` => Some(SchemaResolution(gnuSchema, this))
      case _        => None

  test("absolute ref") {
    val resolved = MySchemaResolver.resolveRef(fooId).map(_.schema)
    assertEquals(resolved, Some(fooSchema))
  }

  test("anchor ref") {
    val resolved = MySchemaResolver.resolveRef("#gnu").map(_.schema)
    assertEquals(resolved, Some(gnuSchema))
  }

  test("path ref") {
    val resolved = MySchemaResolver.resolveRef("#/$defs/gnu").map(_.schema)
    assertEquals(resolved, Some(gnuSchema))
  }

  test("root ref") {
    val resolved = MySchemaResolver.resolveRef("#").map(_.schema)
    assertEquals(resolved, Some(fooSchema))
  }
