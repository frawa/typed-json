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

package frawa.typedjson.js

import frawa.typedjson.{TypedJson}
import frawa.typedjson.TypedJson.Validation
import frawa.typedjson.parser.jawn.JawnParser
import frawa.typedjson.keywords.*
import frawa.typedjson.meta.MetaSchemas
import frawa.typedjson.parser.{Offset, OffsetParser, Value}
import frawa.typedjson.pointer.Pointer
import frawa.typedjson.output.OutputOps
import frawa.typedjson.output.SimpleOutput
import frawa.typedjson.suggest.SuggestOutput
import frawa.typedjson.suggest.Suggest

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportAll, JSExportTopLevel}
import frawa.typedjson.suggest.SuggestResult

@JSExportTopLevel("TypedJsonFactory")
object TypedJsonFactory {
  private val parser = new JawnParser

  @JSExport
  def create(): TypedJsonJS = {
    TypedJsonJS(TypedJson.create())
  }

  @JSExport
  def withMetaSchema(): TypedJsonJS =
    TypedJsonJS(TypedJson.createWithMetaSchemas())

  def parseJsonOffsetValue(json: String): Either[OffsetParser.ParseError, Offset.Value] = {
    parser.parseWithOffset(json)
  }

  def pointerAt(value: Offset.Value, at: Int): Pointer = {
    OffsetParser.pointerAt(value)(at)
  }

  def offsetAt(pointer: Pointer, value: Offset.Value): Option[Offset] = {
    OffsetParser.offsetAt(value)(pointer)
  }
}

@JSExportTopLevel("TypedJson")
case class TypedJsonJS(
    typedJson: TypedJson,
    value: Option[Offset.Value] = None,
    _markers: Seq[Marker] = Seq()
) {
  @JSExport
  def withSchema(schema: TypedJsonJS): TypedJsonJS = {
    schema.value
      .map(Offset.withoutOffset)
      .map(SchemaValue.root)
      .flatMap { schemaValue =>
        val created = TypedJson.createWithSchema(schemaValue)
        // TODO report errors
        created.toOption
      }
      .map { typedJson =>
        this.copy(typedJson = typedJson)
      }
      .getOrElse(this)
  }

  @JSExport
  def forValue(json: String): TypedJsonJS = {
    val parsed = TypedJsonFactory.parseJsonOffsetValue(json)
    parsed match {
      case Right(value) =>
        this.copy(value = Some(value)).validate()
      case Left(error) =>
        this.copy(value = error.recoveredValue, Seq(Marker.fromParsingError(error)))
    }
  }

  private def validate(): TypedJsonJS =
    value
      .flatMap { value =>
        import SimpleOutput.given
        val (o, typedJson) = this.typedJson.eval(value)
        o.map { o =>
          val offsetAt = pointer => TypedJsonFactory.offsetAt(pointer, value)
          copy(_markers = o.errors.map(Marker.fromError(offsetAt)), typedJson = typedJson)
        }
      }
      .getOrElse(this)

  @JSExport
  def markers(): js.Array[Marker] = {
    js.Array(_markers*)
  }

  @JSExport
  def suggestAt(offset: Int): js.Array[Suggestions] = {
    val suggestions = value
      .flatMap { value =>
        val at                         = TypedJsonFactory.pointerAt(value, offset)
        given OutputOps[SuggestOutput] = SuggestOutput.outputOps(at)
        val (o, _)                     = this.typedJson.eval(value)
        o.map { o =>
          val suggestions = Suggest.suggestions(at, o)
          val offsetAt    = pointer => TypedJsonFactory.offsetAt(pointer, value)
          Suggestions(offsetAt)(at, suggestions)
        }
      }
      .map(Seq(_))
      .getOrElse(Seq())
    js.Array(suggestions*)
  }
}

@JSExportTopLevel("Marker")
@JSExportAll
case class Marker(
    start: Int,
    end: Int,
    pointer: String,
    message: String,
    severity: String
)

object Marker {
  def fromSchemaError(error: SchemaProblems.SchemaError): Marker = {
    // TODO localized messages
    val message = error.value.toString
    Marker(0, 0, error.pointer.toString, message, "error")
  }

  def fromError(offsetAt: Pointer => Option[Offset])(error: SimpleOutput.Error): Marker = {
    val offset       = offsetAt(error.pointer)
    val (start, end) = offset.map(o => (o.start, o.end)).getOrElse((0, 0))
    // TODO localized messages
    val message = error.value.toString
    Marker(start, end, error.pointer.toString, message, "error")
  }

  def fromParsingError(error: OffsetParser.ParseError): Marker = {
    Marker(error.offset, error.offset, "", error.message, "error")
  }
}

@JSExportTopLevel("Suggestions")
@JSExportAll
case class Suggestions(
    start: Int,
    end: Int,
    pointer: String,
    suggestions: js.Array[Suggestion]
)

@JSExportTopLevel("Suggestion")
@JSExportAll
case class Suggestion(
    value: js.Any
)

object Suggestions {
  def apply(offsetAt: Pointer => Option[Offset])(pointer: Pointer, result: SuggestResult): Suggestions = {
    val offset       = offsetAt(pointer)
    val (start, end) = offset.map(o => (o.start, o.end)).getOrElse((0, 0))
    val suggestions  = toSuggestions(result)
    Suggestions(start, end, pointer.toString, js.Array(suggestions.toSeq*))
  }

  private def toSuggestions(result: SuggestResult): Seq[Suggestion] =
    result match {
      case SuggestResult.Values(vs)      => vs.map(toSuggestion)
      case SuggestResult.WithMeta(rs, _) => rs.flatMap(toSuggestions)
    }

  private def toSuggestion(value: Value): Suggestion = {
    Suggestion(toAny(value))
  }

  private def toAny(value: Value): js.Any = {
    value match {
      case Value.NumberValue(value) => value.toDouble
      case Value.BoolValue(value)   => value
      case Value.NullValue          => null
      case Value.StringValue(value) => value
      case Value.ArrayValue(items)  => js.Array(items.map(toAny)*)
      case Value.ObjectValue(properties) =>
        val pairs = properties.map(t => (t._1, toAny(t._2))).toSeq
        js.Dictionary(pairs*)
    }
  }
}
