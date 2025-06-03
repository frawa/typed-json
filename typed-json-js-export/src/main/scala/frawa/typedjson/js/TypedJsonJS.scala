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

import frawa.typedjson.TypedJson
import frawa.typedjson.keywords._
import frawa.typedjson.output.OutputJson
import frawa.typedjson.output.OutputOps
import frawa.typedjson.output.SimpleOutput
import frawa.typedjson.parser.Offset
import frawa.typedjson.parser.OffsetContext
import frawa.typedjson.parser.OffsetParser
import frawa.typedjson.parser.Value
import frawa.typedjson.parser.jawn.JawnParser
import frawa.typedjson.pointer.Pointer
import frawa.typedjson.suggest.Suggest
import frawa.typedjson.suggest.Suggest.Doc
import frawa.typedjson.suggest.SuggestOutput
import frawa.typedjson.suggest.SuggestResult

import scala.collection.immutable.Seq
import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.annotation.JSExportAll
import scala.scalajs.js.annotation.JSExportTopLevel

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

  def contextAt(value: Offset.Value, at: Int): OffsetContext = {
    OffsetParser.contextAt(value)(at)
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
        this.copy(value = error.recoveredValue, _markers = Seq(Marker.fromParsingError(error)))
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
  def suggestionsAt(offset: Int): js.UndefOr[SuggestionsResult] = {
    value.flatMap { value =>
      val at                    = TypedJsonFactory.contextAt(value, offset)
      val (keysOnly, atPointer) = at match {
        case _: OffsetContext.InsideKey => (true, at.pointer.outer)
        case _: OffsetContext.NewKey    => (true, at.pointer)
        case _                          => (false, at.pointer)
      }
      // println(("FW", at, keysOnly, atPointer))

      given OutputOps[SuggestOutput] = SuggestOutput.outputOps(atPointer)
      val (o, _)                     = this.typedJson.eval(value)
      o.map { o =>
        val result = Suggest.suggestions(at.pointer, keysOnly, o)
        val offset = at.offset
        val sep    = at match {
          case OffsetContext.NewKey(_, _)   => Some(":")
          case OffsetContext.NewValue(_, _) => Some(",")
          case _                            => None
        }
        SuggestionsResult(at.pointer, offset, sep, result)
      }
    }.orUndefined
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
    val message      = OutputJson.toMessage(error.value)
    Marker(start, end, error.pointer.toString, message, "error")
  }

  def fromParsingError(error: OffsetParser.ParseError): Marker = {
    Marker(error.offset, error.offset, "", error.message, "error")
  }
}

@JSExportTopLevel("SuggestionsResult")
@JSExportAll
case class SuggestionsResult(
    range: TextRange,
    pointer: String,
    suggestions: js.Array[Suggestion]
)

@JSExportTopLevel("Suggestion")
@JSExportAll
case class Suggestion(
    value: js.Any,
    replace: js.UndefOr[TextRange] = js.undefined,
    seperator: js.UndefOr[String] = js.undefined,
    documentationMarkdown: js.UndefOr[String] = js.undefined
)

@JSExportTopLevel("TextRange")
@JSExportAll
case class TextRange(
    start: Int,
    end: Int
)

object SuggestionsResult {
  def apply(
      pointer: Pointer,
      offset: Offset,
      sep: Option[String],
      result: SuggestResult
  ): SuggestionsResult = {
    val range       = toTextRange(offset)
    val suggestions = toSuggestions(result, sep)
    // TODO dedup ealier?
    val dedup = suggestions
      .groupBy(s => js.JSON.stringify(s.value))
      .flatMap(_._2.headOption)
    SuggestionsResult(range, pointer.toString, js.Array(dedup.toSeq*))
  }

  private def toTextRange(offset: Offset): TextRange =
    TextRange(offset.start, offset.end)

  private def toSuggestions(result: SuggestResult, sep: Option[String]): Seq[Suggestion] =

    def go(s: Suggest): Seq[Suggestion] = s match {
      case Suggest.Values(vs)            => vs.map(toSuggestion(sep))
      case Suggest.WithDoc(s, doc)       => go(s).map(withDoc(toMarkdown(doc)))
      case Suggest.WithReplace(s, range) => go(s).map(withReplace(toTextRange(range)))
    }

    result.suggestions.flatMap(go)

  private def toSuggestion(sep: Option[String])(value: Value): Suggestion = {
    Suggestion(toAny(value), seperator = sep.orUndefined)
  }

  private def withDoc(doc: Option[String])(suggestion: Suggestion): Suggestion = {
    suggestion.copy(documentationMarkdown = doc.orUndefined)
  }

  private def withReplace(replace: TextRange)(suggestion: Suggestion): Suggestion = {
    suggestion.copy(replace = replace)
  }

  private def toAny(value: Value): js.Any = {
    value match {
      case Value.NumberValue(value)      => value.toDouble
      case Value.BoolValue(value)        => value
      case Value.NullValue               => null
      case Value.StringValue(value)      => value
      case Value.ArrayValue(items)       => js.Array(items.map(toAny)*)
      case Value.ObjectValue(properties) =>
        val pairs = properties.map(t => (t._1, toAny(t._2))).toSeq
        js.Dictionary(pairs*)
    }
  }

  private def toMarkdown(doc: Doc): Option[String] =
    val md = doc.title
      .zip(doc.description)
      .map { (title, description) =>
        s"""|### ${title}
            |
            |${description}
            |""".stripMargin
      }
      .orElse {
        doc.title.map { title =>
          s"""|### ${title}
              |""".stripMargin
        }
      }
      .orElse {
        doc.description.map { description =>
          s"""|${description}
              |""".stripMargin
        }
      }
    md.zip(doc.id)
      .map { (md, id) =>
        s"""|*[${id}](${id})*
            |${md}
            |""".stripMargin
      }
      .orElse(md)

}
