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
import frawa.typedjson.validation.ValidationOutput
import frawa.typedjson.keywords.InnerValue
import frawa.typedjson.parser.Value
import frawa.typedjson.parser.Offset
import frawa.typedjson.pointer.Pointer
import frawa.typedjson.validation.ValidationError
import frawa.typedjson.keywords.Result

object TypedJson:
  case class Validation(valid: Boolean, output: Output)
  case class Output(errors: Seq[Error]) // TODO add annotations
  case class Error(pointer: Pointer, error: ValidationError)

  trait InputError
  case class JsonError(error: String)               extends InputError
  case class SchemaErrors(problems: SchemaProblems) extends InputError

  def create(): TypedJson = new TypedJson(None)

  def create(schemaJson: String)(using parser: Parser): Either[InputError, TypedJson] =
    parser
      .parse(schemaJson)
      .swap
      .map(JsonError(_))
      .swap
      .flatMap(create(_))

  def create(schema: Value): Either[InputError, TypedJson] =
    withSchema(SchemaValue.root(schema))

  def create(schema: Offset.Value): Either[InputError, TypedJson] =
    withSchema(SchemaValue.root(Offset.withoutOffset(schema)))

  private def withSchema(schema: SchemaValue): Either[InputError, TypedJson] =
    val resolver   = MetaSchemas.lazyResolver
    val vocabulary = Vocabulary.specDialect()
    val keywords   = Keywords(schema, Some(vocabulary), Some(resolver))
    keywords.swap
      .map(SchemaErrors(_))
      .swap
      .map(keywords => new TypedJson(Some(keywords)))

  object Output:
    val empty: Output = Output(Seq.empty)

    def apply(result: Result[ValidationOutput]): Output =
      val errors =
        result.output
          .map(_.errors)
          .getOrElse(Seq())
          .map(error => Error(error.pointer, error.result))
      Output(errors)

class TypedJson(private val keywords: Option[Keywords]):
  import TypedJson._

  def validate(json: String)(using parser: Parser): Either[InputError, Validation] =
    parser
      .parse(json)
      .swap
      .map(JsonError(_))
      .swap
      .map { value =>
        validate(value).getOrElse(Validation(true, Output.empty))
      }

  def validate(value: Value): Option[Validation] =
    keywords
      .map(validator)
      .map { validator => validator(InnerValue(value)) }
      .map { result => Validation(result.valid, Output(result)) }

  def validate(value: Offset.Value): Option[Validation] =
    keywords
      .map(validator)
      .map { validator => validator(InnerValue(Offset.withoutOffset(value))) }
      .map { result => Validation(result.valid, Output(result)) }

  def validator(keywords: Keywords): Evaluator[ValidationOutput] =
    Evaluator(keywords, ValidationProcessing())
