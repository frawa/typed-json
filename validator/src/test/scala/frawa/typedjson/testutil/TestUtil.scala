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

package frawa.typedjson.testutil

import frawa.typedjson.parser.{Parser, Value}
import frawa.typedjson.processor._
import munit.Assertions.{assertEquals, clue, clues, fail}

object TestUtil {
  implicit val lazyResolver: Option[LoadedSchemasResolver.LazyResolver] = None

  def parseJsonValue(text: String)(implicit parser: Parser): Value = {
    parser
      .parse(text)
      .swap
      .map(message => fail("no json value", clues(clue(message))))
      .swap
      .toOption
      .get
  }

  def withSchema(text: String)(f: SchemaValue => Unit)(implicit parser: Parser): Unit = {
    f(SchemaValue(parseJsonValue(text)))
  }

  def withLoadedSchemas(texts: Seq[String])(f: LoadedSchemasResolver => Unit)(implicit parser: Parser): Unit = {
    val schemas  = texts.map(t => parseJsonValue(t)).map(SchemaValue(_))
    val resolver = LoadedSchemasResolver(schemas)
    f(resolver)
  }

  def assertResult[R](
      eval: Eval[R]
  )(schema: SchemaValue, valueText: String, strict: Boolean = true)(
      f: Result[R] => Unit
  )(implicit parser: Parser, lazyResolver: Option[LoadedSchemasResolver.LazyResolver]): Either[Nothing, Unit] = {
    withProcessor(eval)(schema, strict) { processor =>
      val value  = parseJsonValue(valueText)
      val result = processor(InnerValue(value))
      f(result)
    }
  }

  def withProcessor[R](
      eval: Eval[R]
  )(schema: SchemaValue, strict: Boolean = false)(
      f: Processor[R] => Unit
  )(implicit lazyResolver: Option[LoadedSchemasResolver.LazyResolver]): Either[Nothing, Unit] = {
    val result = for {
      processor0 <- Processor(schema, lazyResolver)(eval)
      processor =
        if (strict) {
          processor0.andThen { result =>
            assertEquals(result.ignoredKeywords(), Set.empty[String], "ignored keywords")
            result
          }
        } else {
          processor0
        }
    } yield {
      f(processor)
    }
    result.swap
      .map(message => fail("creating processor failed", clues(clue(message))))
      .swap
  }

  def withStrictProcessor[R](
      eval: Eval[R]
  )(schema: SchemaValue)(
      f: Processor[R] => Unit
  )(implicit lazyResolver: Option[LoadedSchemasResolver.LazyResolver]): Either[Nothing, Unit] = {
    withProcessor(eval)(schema, strict = true)(f)
  }

}
