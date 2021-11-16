package frawa.typedjson.schema

import munit.FunSuite
import UriUtil._
import frawa.typedjson.parser.ZioParser
import java.net.URI

class SpecMetaSchemasTest extends FunSuite {
  implicit val zioParser: ZioParser = new ZioParser()

  val schemaId       = "https://json-schema.org/draft/2020-12/schema"
  val schemaUri: URI = uri(schemaId)
  val coreUri: URI   = schemaUri.resolve("meta/core")

  test("load schema") {
    val schema = SpecMetaSchemas.lazyResolver.apply(schemaUri)
    assertEquals(schema.flatMap(SchemaValue.id(_)), Some(schemaUri.toString))
  }

  test("load relative meta schema") {
    val schema = SpecMetaSchemas.lazyResolver.apply(coreUri)
    assertEquals(schema.flatMap(SchemaValue.id(_)), Some(coreUri.toString))
  }
}
