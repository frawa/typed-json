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
      assertEquals(resolver.base, Some(URI.create("https://example.net/root.json")))
      assertEquals(resolver.resolve("https://example.net/root.json"), Some(schema))
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
      assertEquals(resolver.base, Some(URI.create("https://example.net/root.json")))
      assertEquals(resolver.resolve("https://example.net/root.json"), Some(schema))
      assertEquals(
        resolver.resolve("https://example.net/foo.json"),
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
      assertEquals(resolver.base, Some(URI.create("https://example.net/root.json")))
      assertEquals(resolver.resolve("https://example.net/root.json"), Some(schema))
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
        resolver.resolve("https://example.net/root.json#foo"),
        Some(expected)
      )
    }
  }
}
