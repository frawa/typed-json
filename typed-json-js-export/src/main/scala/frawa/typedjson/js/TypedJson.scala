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

import frawa.typedjson.parser.jawn.JawnParser
import frawa.typedjson.keywords.*
import frawa.typedjson.meta.MetaSchemas
import frawa.typedjson.parser.{Offset, OffsetParser, Value}
import frawa.typedjson.pointer.Pointer
import frawa.typedjson.suggestion.{SuggestionProcessing}
import frawa.typedjson.validation.{ValidationProcessing, ValidationOutput}

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportAll, JSExportTopLevel}
import frawa.typedjson.suggestion.SuggestionOutput

@JSExportTopLevel("TypedJsonFactory")
object TypedJsonFactory {
  private val parser = new JawnParser

  @JSExport
  def create(): TypedJson = {
    TypedJson(None)
  }

  @JSExport
  def withMetaSchema(): TypedJson = {
    val resolver     = MetaSchemas.lazyResolver
    val base         = MetaSchemas.draft202012
    val Some(schema) = resolver(base.resolve("schema")): @unchecked
    val vocabulary   = Vocabulary.specDialect()
    val keywords     = Keywords(schema, Some(vocabulary), Some(resolver))
    TypedJson(Some(keywords))
  }

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
case class TypedJson(
    keywords: Option[Either[SchemaProblems, Keywords]],
    value: Option[Offset.Value] = None,
    result: Option[Either[OffsetParser.ParseError, Result[ValidationOutput]]] = None
) {
  @JSExport
  def withSchema(schema: TypedJson): TypedJson = {
    val resolver    = MetaSchemas.lazyResolver
    val schemaValue = schema.value.map(Offset.withoutOffset).map(SchemaValue.root)
    val vocabulary  = Vocabulary.specDialect()
    val keywords    = schemaValue.map(Keywords(_, Some(vocabulary), Some(resolver)))
    this.copy(keywords = keywords).validate()
  }

  @JSExport
  def forValue(json: String): TypedJson = {
    val parsed = TypedJsonFactory.parseJsonOffsetValue(json)
    parsed match {
      case Right(value) =>
        this.copy(value = Some(value), result = None).validate()
      case Left(error) =>
        this.copy(value = error.recoveredValue, result = Some(Left(error)))
    }
  }

  private def validate(): TypedJson = {
    keywords match {
      case Some(Right(keywords)) =>
        value match {
          case Some(value) =>
            val evaluator = Evaluator(keywords, ValidationProcessing())
            val result    = evaluator(InnerValue(Offset.withoutOffset(value)))
            this.copy(result = Some(Right(result)))
          case None =>
            this
        }
      case _ => this
    }
  }

  @JSExport
  def markers(): js.Array[Marker] = {
    val markers = (keywords, value, result) match {
      case (Some(Right(_)), Some(value), Some(Right(result))) if !result.valid =>
        val offsetAt = pointer => TypedJsonFactory.offsetAt(pointer, value)
        result.output
          .map(_.errors)
          .getOrElse(Seq())
          .map(Marker.fromError(offsetAt))
      case (Some(Left(problems)), _, _) => problems.errors.map(Marker.fromSchemaError) // TODO not needed?
      case (_, _, Some(Left(error))) =>
        Seq(Marker.fromParsingError(error))
      case _ => Seq.empty
    }
    js.Array(markers*)
  }

  @JSExport
  def suggestAt(offset: Int): js.Array[Suggestions] = {
    val suggestions = keywords match {
      case Some(Right(keywords)) =>
        value match {
          case Some(value) =>
            val pointer   = TypedJsonFactory.pointerAt(value, offset)
            val evaluator = Evaluator(keywords, SuggestionProcessing(pointer))
            val result    = evaluator(InnerValue(Offset.withoutOffset(value)))
            val offsetAt  = pointer => TypedJsonFactory.offsetAt(pointer, value)
            Seq(Suggestions(offsetAt)(pointer, result))
          case None =>
            Seq.empty
        }
      case _ => Seq.empty
    }
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

  def fromError(offsetAt: Pointer => Option[Offset])(error: ValidationOutput.Error): Marker = {
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
  def apply(offsetAt: Pointer => Option[Offset])(pointer: Pointer, result: Result[SuggestionOutput]): Suggestions = {
    val offset       = offsetAt(pointer)
    val (start, end) = offset.map(o => (o.start, o.end)).getOrElse((0, 0))
    val suggestions  = result.output.map(_.suggestions).getOrElse(Seq()).map(toSuggestion)
    Suggestions(start, end, pointer.toString, js.Array(suggestions.toSeq*))
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
