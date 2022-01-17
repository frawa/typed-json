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

package frawa.typedjson

import frawa.typedjson.parser.Parser
import frawa.typedjson.keywords.SchemaValue
import frawa.typedjson.meta.MetaSchemas
import frawa.typedjson.keywords.Vocabulary
import frawa.typedjson.keywords.Keywords
import frawa.typedjson.keywords.SchemaProblems
import frawa.typedjson.keywords.Evaluator
import frawa.typedjson.validation.ValidationProcessing
import frawa.typedjson.validation.ValidationResult
import frawa.typedjson.keywords.InnerValue
import frawa.typedjson.parser.Value
import frawa.typedjson.parser.Offset

object TypedJson {
  case class Validation(valid: Boolean)

  trait Error
  case class JsonError(error: String)               extends Error
  case class SchemaErrors(problems: SchemaProblems) extends Error

  def create(): TypedJson = new TypedJson(None)

  def create(schemaJson: String)(implicit parser: Parser): Either[Error, TypedJson] = {
    parser
      .parse(schemaJson)
      .swap
      .map(JsonError(_))
      .swap
      .flatMap(create(_))
  }

  def create(schema: Value): Either[Error, TypedJson] = {
    withSchema(SchemaValue.root(schema))
  }

  def create(schema: Offset.Value): Either[Error, TypedJson] = {
    withSchema(SchemaValue.root(Offset.withoutOffset(schema)))
  }

  private def withSchema(schema: SchemaValue): Either[Error, TypedJson] = {
    val resolver   = MetaSchemas.lazyResolver
    val vocabulary = Vocabulary.specDialect()
    val keywords   = Keywords(schema, Some(vocabulary), Some(resolver))
    keywords.swap
      .map(SchemaErrors(_))
      .swap
      .map(keywords => new TypedJson(Some(keywords)))
  }

}

class TypedJson(private val keywords: Option[Keywords]) {
  import TypedJson._

  def validate(json: String)(implicit parser: Parser): Either[Error, Validation] = {
    parser
      .parse(json)
      .swap
      .map(JsonError(_))
      .swap
      .map { value =>
        validate(value).getOrElse(Validation(true))
      }
  }

  def validate(value: Value): Option[Validation] = {
    keywords
      .map(validator)
      .map { validator => validator(InnerValue(value)) }
      .map { result => Validation(result.valid) }
  }

  def validate(value: Offset.Value): Option[Validation] = {
    keywords
      .map(validator)
      .map { validator => validator(InnerValue(Offset.withoutOffset(value))) }
      .map { result => Validation(result.valid) }
  }

  def validator(keywords: Keywords): Evaluator[ValidationResult] = {
    Evaluator(keywords, ValidationProcessing())
  }

}
