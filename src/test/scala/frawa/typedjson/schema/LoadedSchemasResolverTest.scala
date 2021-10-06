package frawa.typedjson.schema

import munit.FunSuite
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
import munit.Assertions._
import java.net.URI
import TestUtil._

class LoadedSchemasResolverTest extends FunSuite {
  implicit val zioParser = new ZioParser()

  test("first schema loader") {
    withSchema("""{
                 |"$id": "https://example.net/root.json",
                 |"type": "null"
                 |}""".stripMargin) { schema =>
      val resolver = LoadedSchemasResolver(schema)
      val uri      = URI.create("https://example.net/root.json")
      assertEquals(resolver.base, Some(uri))
      assertEquals(resolver.resolve(uri).map(_._1), Some(schema))
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
      assertEquals(resolver.base, Some(URI.create("")))
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
        resolver.resolveRef("#/$defs/foo").map(_._1),
        Some(expected)
      )
    }
  }

  test("$defs with $id") {
    withSchema("""{
                 |"$id": "https://example.net/root.json",
                 |"type": "null",
                 |"$defs": {
                 |    "foo": {
                 |        "$id": "https://example.net/foo.json",
                 |        "type": "number"
                 |    }
                 |}
                 |}""".stripMargin) { schema =>
      val resolver = LoadedSchemasResolver(schema)
      val uri      = URI.create("https://example.net/root.json")
      assertEquals(resolver.base, Some(uri))
      assertEquals(resolver.schemas.size, 2)
      assertEquals(resolver.resolve(uri).map(_._1), Some(schema))

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
        resolver.resolveRef("https://example.net/root.json#/$defs/foo").map(_._1),
        Some(expected)
      )

      assertEquals(
        resolver.resolveRef("https://example.net/foo.json").map(_._1),
        Some(expected)
      )
    }
  }

  test("anchor") {
    withSchema("""{
                 |"$id": "https://example.net/root.json",
                 |"type": "null",
                 |"$defs": {
                 |    "foo": {
                 |        "$anchor": "foo",
                 |        "type": "number"
                 |    }
                 |}
                 |}""".stripMargin) { schema =>
      val resolver = LoadedSchemasResolver(schema)
      val uri      = URI.create("https://example.net/root.json")
      assertEquals(resolver.base, Some(uri))
      assertEquals(resolver.resolve(uri).map(_._1), Some(schema))
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
        resolver.resolve(URI.create("https://example.net/root.json#foo")).map(_._1),
        Some(expected)
      )
    }
  }

  test("$dynamicRef") {
    withLoadedSchemas(
      List(
        """{
          |"$id": "https://example.net/root1",
          |"type": "null",
          |"$dynamicAnchor": "anchor1"
          |}""".stripMargin,
        """{
          |"$id": "https://example.net/root2",
          |"$dynamicAnchor": "anchor1",
          |"$ref": "root1"
          |}""".stripMargin
      )
    ) { resolver =>
      val uriRoot2 = URI.create("https://example.net/root2")
      val uriRoot1 = URI.create("https://example.net/root1")
      val id       = Pointer.empty / "$id"

      val scope1 = DynamicScope.empty.push(uriRoot1)
      val scope2 = DynamicScope.empty.push(uriRoot2).push(uriRoot1)

      val Some((id1, id2)) = for {
        (_, resolver1) <- resolver.resolveRef(uriRoot2.toString)
        (_, resolver2) <- resolver1.resolveRef("root1")
        (schema1, _)   <- resolver2.resolveDynamicRef("#anchor1", scope2)
        (schema2, _)   <- resolver1.resolveDynamicRef("#anchor1", scope1)
        id1 = id(schema1.value)
        id2 = id(schema2.value)
      } yield ((id1, id2))
      assertEquals(id1, Some(StringValue("https://example.net/root2")))
      assertEquals(id2, Some(StringValue("https://example.net/root1")))
    // assertEquals(scope4, Seq(uriRoot2, uriRoot1))
    // assertEquals(scope5, Seq(uriRoot1))
    }
  }

  test("$dynamicAnchor") {
    withLoadedSchemas(
      List("""|{
              |  "$id": "https://example.net/root",
              |  "$ref": "list",
              |  "$defs": {
              |    "foo": {
              |      "$anchor": "items",
              |      "type": "string"
              |    },
              |    "list": {
              |      "$id": "list",
              |      "type": "array",
              |      "items": { "$dynamicRef": "#items" },
              |      "$defs": {
              |        "items": {
              |          "$comment": "This is only needed to satisfy the bookending requirement",
              |          "$dynamicAnchor": "items"
              |        }
              |      }
              |    }
              |  }
              |}""".stripMargin)
    ) { resolver =>
      val rootId  = "https://example.net/root"
      val uriRoot = URI.create(rootId)

      assertEquals(resolver.dynamicSchemas.keySet, Set(URI.create("list#items")))
      assertEquals(
        resolver.schemas.keySet,
        Set(
          uriRoot,
          URI.create("https://example.net/root#items"),
          URI.create("list")
        )
      )

      val id      = Pointer.empty / "$id"
      val anchor  = Pointer.empty / "$anchor"
      val comment = Pointer.empty / "$comment"

      val scope = DynamicScope.empty.push(rootId)
      val ok = for {
        (schema1, resolver1) <- resolver.resolveRef(rootId)
        StringValue(id1)     <- id(schema1.value)
        (schema2, resolver2) <- resolver1.resolveDynamicRef("#items", scope)
        StringValue(anchor2) <- anchor(schema2.value)
      } yield {
        assertEquals(id1, rootId)
        assertEquals(anchor2, "items")
        true
      }
      ok.getOrElse {
        fail("unexpected None")
        false
      }
    }
  }

  test("$dynamicAnchor from root") {
    withLoadedSchemas(
      List("""|{
              |  "$id": "https://example.net/root",
              |  "$ref": "list",
              |  "$defs": {
              |    "foo": {
              |      "$dynamicAnchor": "items",
              |      "type": "string"
              |    },
              |    "list": {
              |      "$id": "list",
              |      "type": "array",
              |      "items": { "$dynamicRef": "#items" },
              |      "$defs": {
              |        "items": {
              |          "$comment": "This is only needed to satisfy the bookending requirement",
              |          "$dynamicAnchor": "items"
              |        }
              |      }
              |    }
              |  }
              |}""".stripMargin)
    ) { resolver =>
      val rootId  = "https://example.net/root"
      val uriRoot = URI.create(rootId)

      assertEquals(
        resolver.dynamicSchemas.keySet,
        Set(URI.create("https://example.net/root#items"), URI.create("list#items"))
      )
      assertEquals(
        resolver.schemas.keySet,
        Set(
          uriRoot,
          URI.create("list")
        )
      )

      val id            = Pointer.empty / "$id"
      val dynamicAnchor = Pointer.empty / "$dynamicAnchor"
      val `type`        = Pointer.empty / "type"

      val scope = DynamicScope.empty.push(rootId)
      val ok = for {
        (schema1, resolver1)        <- resolver.resolveRef(rootId)
        StringValue(id1)            <- id(schema1.value)
        (schema2, resolver2)        <- resolver1.resolveDynamicRef("#items", scope)
        StringValue(dynamicAnchor2) <- dynamicAnchor(schema2.value)
        StringValue(type2)          <- `type`(schema2.value)
      } yield {
        assertEquals(id1, rootId)
        assertEquals(dynamicAnchor2, "items")
        assertEquals(type2, "string")
        true
      }
      ok.getOrElse {
        fail("unexpected None")
        false
      }
    }
  }
}
