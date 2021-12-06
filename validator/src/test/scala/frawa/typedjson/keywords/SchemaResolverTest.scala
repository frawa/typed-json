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

import munit.FunSuite
import frawa.typedjson.parser.ObjectValue
import java.net.URI
import frawa.typedjson.util.UriUtil._
import frawa.typedjson.parser.StringValue

class SchemaResolverTest extends FunSuite {
  val fooId       = "https://example.net/foo.json"
  val fooUri: URI = uri(fooId)

  val gnuUri: URI = uri("https://example.net/foo.json#gnu")
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

  case object MySchemaResolver extends SchemaResolver {

    override val base: URI = fooUri

    override def resolve(uri: URI): Option[Resolution] = uri match {
      case `fooUri` => Some((fooSchema, this))
      case `gnuUri` => Some((gnuSchema, this))
      case _        => None
    }

  }

  test("absolute ref") {
    val resolved = MySchemaResolver.resolveRef(fooId).map(_._1)
    assertEquals(resolved, Some(fooSchema))
  }

  test("anchor ref") {
    val resolved = MySchemaResolver.resolveRef("#gnu").map(_._1)
    assertEquals(resolved, Some(gnuSchema))
  }

  test("path ref") {
    val resolved = MySchemaResolver.resolveRef("#/$defs/gnu").map(_._1)
    assertEquals(resolved, Some(gnuSchema))
  }

  test("root ref") {
    val resolved = MySchemaResolver.resolveRef("#").map(_._1)
    assertEquals(resolved, Some(fooSchema))
  }

}
