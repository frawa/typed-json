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
import UriUtil._

class SchemaResolverTest extends FunSuite {
  val fooId  = "https://example.net/foo.json"
  val fooUri = uri(fooId)

  val gnuUri = uri("https://example.net/foo.json#gnu")
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

    override val base = fooUri
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
