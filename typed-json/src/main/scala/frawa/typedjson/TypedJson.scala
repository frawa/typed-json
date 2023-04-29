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
import frawa.typedjson.parser.Value
import frawa.typedjson.parser.Offset
import frawa.typedjson.pointer.Pointer
import frawa.typedjson.validation.ValidationError

import frawa.typedjson.eval.CacheState.R
import frawa.typedjson.eval.TheResultMonad
import frawa.typedjson.eval.Eval
import frawa.typedjson.output.BasicOutput
import frawa.typedjson.eval.CacheState
import frawa.typedjson.keywords.LoadedSchemasResolver
import frawa.typedjson.keywords.SchemaResolver
import frawa.typedjson.parser.OffsetParser
import frawa.typedjson.suggest.Suggest
import frawa.typedjson.output.OutputOps
import frawa.typedjson.suggest.SuggestOutput

case class TypedJson(private val state: Option[(Keywords, CacheState)]):
  import TypedJson.*

  def eval[O: OutputOps](value: Offset.Value): (Option[O], TypedJson) =
    doEval(Offset.withoutOffset(value))

  def eval[O: OutputOps](value: Value): (Option[O], TypedJson) =
    doEval(value)

  def evalBulk[O: OutputOps](values: Seq[Value]): (Seq[O], TypedJson) =
    state
      .map { (keywords, cache) =>
        val eval: Eval[R, O] = Eval[R, O]
        val compiled         = eval.compile(keywords)
        val fun              = eval.fun(compiled)

        val (os, cache1) = values.foldLeft((Seq.empty[O], cache)) { case ((os, cache), value) =>
          val (o, cache1) = fun(value)(cache)
          (os :+ o, cache1)
        }
        (os, cache1)
      }
      .map { (os, cache) =>
        (os, copy(state = state.map { (k, _) => (k, cache) }))
      }
      .getOrElse((Seq(), this))

  private def doEval[O: OutputOps](value: Value): (Option[O], TypedJson) =
    state
      .map { (keywords, cache) =>
        val eval: Eval[R, O] = Eval[R, O]
        val compiled         = eval.compile(keywords)
        val fun              = eval.fun(compiled)

        val (o, cache1) = fun(value)(cache)
        // TODO log stats?

        (o, cache1)
      }
      .map { (o, cache) =>
        (Some(o), copy(state = state.map { (k, _) => (k, cache) }))
      }
      .getOrElse((None, this))

object TypedJson:
  type EvalFun[O] = Eval.EvalFun[R, O]

  case class Validation(valid: Boolean, output: Output)
  case class Output(errors: Seq[Error]) // TODO add annotations
  case class Error(pointer: Pointer, error: ValidationError)

  trait InputError
  case class JsonError(error: String)               extends InputError
  case class SchemaErrors(problems: SchemaProblems) extends InputError

  def create(): TypedJson = new TypedJson(None)

  def create(
      schemaJson: String
  )(using parser: Parser): Either[InputError, TypedJson] =
    parser
      .parse(schemaJson)
      .swap
      .map(JsonError(_))
      .swap
      .flatMap(create(_))

  def create(schema: Value): Either[InputError, TypedJson] =
    createWithSchema(SchemaValue.root(schema))

  def create(schema: Offset.Value): Either[InputError, TypedJson] =
    createWithSchema(SchemaValue.root(Offset.withoutOffset(schema)))

  def createWithMetaSchemas(): TypedJson =
    val lazyResolver = MetaSchemas.lazyResolver
    val base         = MetaSchemas.draft202012
    val Some(schema) = lazyResolver(base.resolve("schema")): @unchecked
    createWithSchema(schema).getOrElse(throw new IllegalStateException("broken meta schemas"))

  def createWithSchema(schema: SchemaValue): Either[InputError, TypedJson] =
    val lazyResolver                   = MetaSchemas.lazyResolver
    val vocabulary                     = Vocabulary.specDialect()
    val keywords                       = Keywords(schema, Some(vocabulary), Some(lazyResolver))
    val schemaResolver: SchemaResolver = LoadedSchemasResolver(schema, Some(lazyResolver))
    keywords.swap
      .map(SchemaErrors(_))
      .swap
      .map { keywords =>
        val cache = CacheState.empty(schemaResolver, keywords.vocabulary)
        new TypedJson(Some(keywords, cache))
      }

  private def compile[R[_], O](keywords: Keywords)(using eval: Eval[R, O])(using
      TheResultMonad[R, O]
  ): Eval.EvalFun[R, O] =
    val compiled = eval.compile(keywords)
    eval.fun(compiled)

  object Output:
    val empty: Output = Output(Seq.empty)

    def apply(o: BasicOutput): Output =
      val errors =
        o.errors.map(error => Error(error.pointer, error.value))
      Output(errors)
