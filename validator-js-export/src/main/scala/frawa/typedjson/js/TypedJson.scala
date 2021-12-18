package frawa.typedjson.js

import frawa.typedjson.keywords.{Keywords, SchemaValue}
import frawa.typedjson.meta.MetaSchemas
import frawa.typedjson.parser.{JawnParser, Offset, Value}
import frawa.typedjson.pointer.Pointer

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("SuggestFactory")
object SuggestFactory {
  private val parser = new JawnParser

  @JSExport
  def withSchema(json: String): Suggest = {
    val suggest = for {
      value <- parseJsonValue(json)
      schema = SchemaValue.root(value)
    } yield {
      Suggest(schema)
    }
    suggest.swap
      .map(error => throw new IllegalArgumentException("broken schema: " + error))
      .swap
      .toOption
      .get
  }

  @JSExport
  def withMetaSchema(): Suggest = {
    val resolver     = MetaSchemas.lazyResolver
    val base         = MetaSchemas.draft202012
    val Some(schema) = resolver(base.resolve("schema"))
    Suggest(schema)
  }

  private def parseJsonValue(json: String): Either[String, Value] = {
    parser.parse(json)
  }

  def parseJsonOffsetValue(json: String): Either[String, Offset.Value] = {
    parser.parseWithOffset(json)
  }

  def pointerAt(value: Offset.Value, at: Int): Pointer = {
    parser.pointerAt(value)(at)
  }

}

@JSExportTopLevel("Suggest")
case class Suggest(schema: SchemaValue, value: Option[Offset.Value] = None) {
  private val keywords = Keywords(schema, None, None)

  @JSExport
  def forValue(json: String): Suggest = {
    println("Hello world!", json)
    val suggest = for {
      value <- SuggestFactory.parseJsonOffsetValue(json)
    } yield {
      this.copy(value = Some(value))
    }
    suggest.getOrElse(this)
  }

  @JSExport
  def at(offset: Int): js.Array[String] = {
    val pointer = value.map(SuggestFactory.pointerAt(_, offset))
    println("Hello world, again!", offset, pointer)
    js.Array(Seq("hello", "world"): _*)
  }

}
