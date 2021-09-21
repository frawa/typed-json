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
  implicit val zioParser = new ZioParser();

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
      assertEquals(resolver.resolve(uri).map(_._1), Some(schema))
      assertEquals(
        resolver.resolve(URI.create("https://example.net/foo.json")).map(_._1),
        Some(
          value = SchemaValue(
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
        )
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

  test("scope") {
    withLoadedSchemas(
      List(
        """{
          |"$id": "https://example.net/root1",
          |"type": "null"
          |}""".stripMargin,
        """{
          |"$id": "https://example.net/root2",
          |"$ref": "root1"
          |}""".stripMargin
      )
    ) { resolver =>
      assertEquals(resolver.scope, Seq())
      val uriRoot2 = URI.create("https://example.net/root2")
      val uriRoot1 = URI.create("https://example.net/root1")
      val Some((scope1, scope2)) = for {
        (schema1, resolver1) <- resolver.resolveRef(uriRoot2.toString)
        (schema2, resolver2) <- resolver1.resolveRef("root1")
      } yield ((resolver1.scope, resolver2.scope))
      assertEquals(scope1, Seq(uriRoot2))
      assertEquals(scope2, Seq(uriRoot2, uriRoot1))
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
      assertEquals(resolver.scope, Seq())
      val uriRoot2 = URI.create("https://example.net/root2")
      val uriRoot1 = URI.create("https://example.net/root1")
      val id       = Pointer.empty / "$id"
      val Some((id4, id5, scope4, scope5)) = for {
        (schema1, resolver1) <- resolver.resolveRef(uriRoot1.toString)
        (schema2, resolver2) <- resolver.resolveRef(uriRoot2.toString)
        (schema3, resolver3) <- resolver2.resolveRef("root1")
        (schema4, resolver4) <- resolver3.resolveDynamicRef("#anchor1")
        (schema5, resolver5) <- resolver1.resolveDynamicRef("#anchor1")
        id4    = id(schema4.value)
        id5    = id(schema5.value)
        scope4 = resolver4.scope
        scope5 = resolver5.scope
      } yield ((id4, id5, scope4, scope5))
      assertEquals(id4, Some(StringValue("https://example.net/root2")))
      assertEquals(id5, Some(StringValue("https://example.net/root1")))
      assertEquals(scope4, Seq(uriRoot2, uriRoot1))
      assertEquals(scope5, Seq(uriRoot1))
    }
  }
}

class SchemaResolverTest extends FunSuite {
  val fooId  = "https://example.net/foo.json"
  val fooUri = URI.create(fooId)

  val gnuUri = URI.create("https://example.net/foo.json#gnu")
  val gnuSchema =
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

  val fooSchema =
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

    override val base: Option[URI] = Some(fooUri)
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
}
