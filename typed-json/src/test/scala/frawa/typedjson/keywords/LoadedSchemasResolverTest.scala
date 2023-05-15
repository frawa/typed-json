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

import frawa.typedjson.parser.Value.*
import frawa.typedjson.pointer.Pointer
import frawa.typedjson.testutil.TestUtil.{*, given}
import frawa.typedjson.util.UriUtil.*
import munit.FunSuite
import frawa.typedjson.keywords.LoadedSchemasResolver.LazyResolver

class LoadedSchemasResolverTest extends FunSuite:

  test("first schema loader") {
    val id = "https://example.net/root.json"
    withSchema(s"""{
                  |"$$id": "$id",
                  |"type": "null"
                  |}""".stripMargin) { schema =>
      val resolver = LoadedSchemasResolver(schema)
      val uri1     = uri(id)
      assertEquals(resolver.base, uri1)
      assertEquals(resolver.resolveRef(id).map(_.schema.value), Some(schema.value))
    }
  }

  test("urn:uuid id resolving $ref from internal $defs") {
    val id = "urn:uuid:feebdaed-ffff-0000-ffff-0000deadbeef/"
    withSchema(s"""{
                  |"$$id": "$id",
                  |"$$defs": {"bar": {"type": "string"}},
                  |"$$ref": "#/$$defs/bar"
                  |}""".stripMargin) { schema =>
      val resolver = LoadedSchemasResolver(schema)
      val uri1     = uri(id)
      assertEquals(resolver.base, uri1)
      assertEquals(resolver.resolveRef(id).map(_.schema.value), Some(schema.value))
      assertEquals(
        resolver.resolveRef("#/$defs/bar").map(_.schema.value),
        Some(
          ObjectValue(
            Map("type" -> StringValue("string"))
          )
        )
      )
    }
  }

  test("$defs") {
    withSchema("""{
                 |"type": "null",
                 |"$defs": {
                 |    "foo": {
                 |        "type": "number"
                 |    }
                 |}
                 |}""".stripMargin) { schema =>
      val resolver = LoadedSchemasResolver(schema)
      assertEquals(resolver.base, uri(""))
      assertEquals(resolver.schemas.size, 1)

      val expected = SchemaValue(
        value = ObjectValue(
          properties = Map(
            "type" -> StringValue(
              value = "number"
            )
          )
        )
      )

      assertEquals(
        resolver.resolveRef("#/$defs/foo").map(_.schema),
        Some(expected)
      )
    }
  }

  test("$defs with $id") {
    val id = "https://example.net/root.json"
    withSchema(s"""{
                  |"$$id": "$id",
                  |"type": "null",
                  |"$$defs": {
                  |    "foo": {
                  |        "$$id": "https://example.net/foo.json",
                  |        "type": "number"
                  |    }
                  |}
                  |}""".stripMargin) { schema =>
      val resolver = LoadedSchemasResolver(schema)
      val uri1     = uri(id)
      assertEquals(resolver.base, uri1)
      assertEquals(resolver.schemas.size, 2)
      assertEquals(resolver.resolveRef(id).map(_.schema.value), Some(schema.value))

      val expected = SchemaValue(
        value = ObjectValue(
          properties = Map(
            "$id" -> StringValue(
              value = "https://example.net/foo.json"
            ),
            "type" -> StringValue(
              value = "number"
            )
          )
        )
      )

      assertEquals(
        resolver.resolveRef("https://example.net/root.json#/$defs/foo").map(_.schema),
        Some(expected)
      )

      assertEquals(
        resolver.resolveRef("https://example.net/foo.json").map(_.schema),
        Some(expected)
      )
    }
  }

  test("anchor") {
    val id = "https://example.net/root.json"
    withSchema(s"""{
                  |"$$id": "$id",
                  |"type": "null",
                  |"$$defs": {
                  |    "foo": {
                  |        "$$anchor": "foo",
                  |        "type": "number"
                  |    }
                  |}
                  |}""".stripMargin) { schema =>
      val resolver = LoadedSchemasResolver(schema)
      val uri1     = uri(id)
      assertEquals(resolver.base, uri1)
      assertEquals(resolver.resolveRef(id).map(_.schema.value), Some(schema.value))
      val expected = SchemaValue(
        value = ObjectValue(
          properties = Map(
            "$anchor" -> StringValue(
              value = "foo"
            ),
            "type" -> StringValue(
              value = "number"
            )
          )
        )
      )

      assertEquals(
        resolver.resolveRef(s"$id#foo").map(_.schema),
        Some(expected)
      )
    }
  }

  test("$dynamicRef") {
    val id1 = "https://example.net/root1"
    val id2 = "https://example.net/root2"
    withLoadedSchemas(
      List(
        s"""{
           |"$$id": "$id1",
           |"type": "null",
           |"$$dynamicAnchor": "anchor1"
           |}""".stripMargin,
        s"""{
           |"$$id": "$id2",
           |"$$dynamicAnchor": "anchor1"
           |}""".stripMargin
      )
    ) { resolver =>
      val uriRoot1 = uri(id1)
      val uriRoot2 = uri(id2)

      val scope1 = DynamicScope.empty.push(uriRoot1)
      val scope2 = DynamicScope.empty.push(uriRoot2).push(uriRoot1)

      val resolver1 = resolver.withBase(uriRoot1)

      val Some((idA, idB)) = (for
        SchemaResolution(schemaA, _) <- resolver1.resolveDynamicRef("#anchor1", scope2)
        SchemaResolution(schemaB, _) <- resolver1.resolveDynamicRef("#anchor1", scope1)
        idA                          <- SchemaValue.id(schemaA)
        idB                          <- SchemaValue.id(schemaB)
      yield (idA, idB)): @unchecked
      assertEquals(idA, id2)
      assertEquals(idB, id1)
    }
  }

  test("$dynamicAnchor") {
    val id = "https://example.net/root"
    withLoadedSchemas(
      List(s"""|{
               |  "$$id": "$id",
               |  "$$ref": "list",
               |  "$$defs": {
               |    "foo": {
               |      "$$anchor": "items",
               |      "type": "string"
               |    },
               |    "list": {
               |      "$$id": "list",
               |      "type": "array",
               |      "items": { "$$dynamicRef": "#items" },
               |      "$$defs": {
               |        "items": {
               |          "$$comment": "This is only needed to satisfy the bookending requirement",
               |          "$$dynamicAnchor": "items"
               |        }
               |      }
               |    }
               |  }
               |}""".stripMargin)
    ) { resolver =>
      val uriRoot = uri(id)

      assertEquals(resolver.dynamicSchemas, Set(uriRoot.resolve(uri("list#items"))))
      assertEquals(
        resolver.schemas.keySet,
        Set(
          uriRoot,
          uri(s"$id#items"),
          uriRoot.resolve(uri("list")),
          uriRoot.resolve(uri("list#items"))
        )
      )

      val getAnchor = Pointer.empty / "$anchor"
      Pointer.empty / "$comment"

      val scope = DynamicScope.empty.push(uriRoot)
      val ok = for
        SchemaResolution(schema1, resolver1) <- resolver.resolveRef(id)
        id1                                  <- SchemaValue.id(schema1)
        SchemaResolution(schema2, _)         <- resolver1.resolveDynamicRef("#items", scope)
        case StringValue(anchor2) <- getAnchor(schema2.value)
      yield
        assertEquals(id1, id)
        assertEquals(anchor2, "items")
        true
      ok.getOrElse {
        fail("unexpected None")
        false
      }
    }
  }

  test("$dynamicAnchor from root") {
    val id = "https://example.net/root"
    withLoadedSchemas(
      List(s"""|{
               |  "$$id": "$id",
               |  "$$ref": "list",
               |  "$$defs": {
               |    "foo": {
               |      "$$dynamicAnchor": "items",
               |      "type": "string"
               |    },
               |    "list": {
               |      "$$id": "list",
               |      "type": "array",
               |      "items": { "$$dynamicRef": "#items" },
               |      "$$defs": {
               |        "items": {
               |          "$$comment": "This is only needed to satisfy the bookending requirement",
               |          "$$dynamicAnchor": "items"
               |        }
               |      }
               |    }
               |  }
               |}""".stripMargin)
    ) { resolver =>
      val uriRoot = uri(id)

      assertEquals(
        resolver.dynamicSchemas,
        Set(uri(s"$id#items"), uriRoot.resolve(uri("list#items")))
      )
      assertEquals(
        resolver.schemas.keySet,
        Set(
          uriRoot,
          uri("https://example.net/root"),
          uri("https://example.net/root#items"),
          uriRoot.resolve(uri("list")),
          uriRoot.resolve(uri("list#items"))
        )
      )

      val getDynamicAnchor = Pointer.empty / "$dynamicAnchor"
      val getType          = Pointer.empty / "type"

      val scope = DynamicScope.empty.push(uriRoot)
      val ok = for
        SchemaResolution(schema1, resolver1) <- resolver.resolveRef(id)
        id1                                  <- SchemaValue.id(schema1)
        SchemaResolution(schema2, _)         <- resolver1.resolveDynamicRef("#items", scope)
        case StringValue(dynamicAnchor2) <- getDynamicAnchor(schema2.value)
        case StringValue(type2) <- getType(schema2.value)
      yield
        assertEquals(id1, id)
        assertEquals(dynamicAnchor2, "items")
        assertEquals(type2, "string")
        true
      ok.getOrElse {
        fail("unexpected None")
        false
      }
    }
  }

  test("caching lazy resolutions") {
    withLoadedSchemas(Seq()) { resolver =>
      assertEquals(resolver.resolveRef("missing"), None)

      val resolver1 = resolver.withLazyResolver(_ => Some(SchemaValue.root(NullValue)))

      val Some(SchemaResolution(schema, resolver2)) = resolver1.resolveRef("cache-me"): @unchecked
      assertEquals(schema, SchemaValue.root(NullValue))
      assertEquals(resolver2.isInstanceOf[LoadedSchemasResolver], true)
      assertEquals(resolver2.asInstanceOf[LoadedSchemasResolver].schemas.keySet, Set(uri("cache-me")))
    }
  }

  test("with meta $schema") {
    val id     = "https://example.net/root.json"
    val metaId = "https://example.net/meta.json"
    withSchema(s"""{
                  |"$$id": "$id",
                  |"$$schema": "$metaId",
                  |"type": "null"
                  |}""".stripMargin) { schema =>
      val RootSchemaValue(_, Some(uri1)) = schema: @unchecked
      assertEquals(uri(metaId), uri1)
    }
  }

  test("lazy resolve with fragment") {
    val exampleUri = "https://example.net/example.json"
    val ref        = s"${exampleUri}#foo"
    withSchema("""|{
                  |    "$defs": {
                  |        "refToInteger": {
                  |            "$ref": "#foo"
                  |        },
                  |        "A": {
                  |            "$anchor": "foo",
                  |            "type": "integer"
                  |        }
                  |    }
                  |}""".stripMargin) { exampleSchema =>
      withLoadedSchemas(Seq()) { resolver0 =>
        val uriExample = uri(exampleUri)
        val lazyResolver: LazyResolver = uri =>
          if uri.toString == exampleUri then Some(RootSchemaValue(exampleSchema.value, Some(uriExample)))
          else None
        val resolver = resolver0.withLazyResolver(lazyResolver)

        val Some(schema, resolver1) = resolver.resolveRef(ref): @unchecked
        assertEquals(resolver1.base, uri(ref))
        assertEquals(
          schema.value,
          ObjectValue(
            Map(
              "$anchor" -> StringValue("foo"),
              "type"    -> StringValue("integer")
            )
          )
        )
      }
    }
  }
