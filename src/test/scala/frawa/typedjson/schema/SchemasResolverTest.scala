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
      assertEquals(resolver.resolve(uri), Some(schema))
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
      assertEquals(resolver.resolve(uri), Some(schema))
      assertEquals(
        resolver.resolve(URI.create("https://example.net/foo.json")),
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
      assertEquals(resolver.resolve(uri), Some(schema))
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
        resolver.resolve(URI.create("https://example.net/root.json#foo")),
        Some(expected)
      )
    }
  }
}

class SchemaResolverTest extends FunSuite {
  val fooId  = "https://example.net/foo.json"
  val fooUri = URI.create(fooId)
  val fooSchema =
    SchemaValue(
      value = ObjectValue(
        properties = Map(
          "$id" -> StringValue(
            value = fooId
          ),
          "type" -> StringValue(
            value = "number"
          )
        )
      )
    )

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

  case object NySchemaResolver extends SchemaResolver {

    override val base: Option[URI] = Some(fooUri)
    override def resolve(uri: URI): Option[SchemaValue] = uri match {
      case `fooUri` => Some(fooSchema)
      case `gnuUri` => Some(gnuSchema)
      case _        => None
    }
  }

  test("absolute ref") {
    val resolved = NySchemaResolver.resolveRef(fooId)
    assertEquals(resolved, Some(fooSchema))
  }

  test("anchor ref") {
    val resolved = NySchemaResolver.resolveRef("#gnu")
    assertEquals(resolved, Some(gnuSchema))
  }
}
