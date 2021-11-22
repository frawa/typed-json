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

package frawa.typedjson.processor

import munit.FunSuite
import frawa.typedjson.util.UriUtil._
import frawa.typedjson.meta.MetaSchemas
import frawa.typedjson.parser.ZioParser

import java.net.URI

class SpecMetaSchemasTest extends FunSuite {
  implicit val zioParser: ZioParser = new ZioParser()

  val schemaId       = "https://json-schema.org/draft/2020-12/schema"
  val schemaUri: URI = uri(schemaId)
  val coreUri: URI   = schemaUri.resolve("meta/core")

  test("load schema") {
    val schema = MetaSchemas.lazyResolver.apply(schemaUri)
    assertEquals(schema.flatMap(SchemaValue.id(_)), Some(schemaUri.toString))
  }

  test("load relative meta schema") {
    val schema = MetaSchemas.lazyResolver.apply(coreUri)
    assertEquals(schema.flatMap(SchemaValue.id(_)), Some(coreUri.toString))
  }
}
