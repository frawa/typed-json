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

import frawa.typedjson.keywords.*
import frawa.typedjson.parser.jawn.JawnParser
import frawa.typedjson.parser.{Parser, Value}
import munit.Assertions.{assertEquals, clue, clues, fail}

import java.net.URI

object TestUtil:
  given Parser                                     = new JawnParser
  given Option[LoadedSchemasResolver.LazyResolver] = None

  def parseJsonValue(text: String)(using parser: Parser): Value =
    parser
      .parse(text)
      .swap
      .map(message => fail("no json value", clues(clue[String](message))))
      .swap
      .toOption
      .get

  def withSchema(text: String)(f: SchemaValue => Unit)(using parser: Parser): Unit =
    f(SchemaValue.root(parseJsonValue(text)))

  def withLoadedSchemas(texts: Seq[String])(f: LoadedSchemasResolver => Unit)(using Parser): Unit =
    val schemas  = texts.map(t => parseJsonValue(t)).map(SchemaValue(_))
    val resolver = LoadedSchemasResolver(schemas)
    f(resolver)

  // TODO
  def assertNoIgnoredKeywords() = ???

  def dialect(vocabularyIds: Seq[URI]): Option[Vocabulary] =
    Vocabulary
      .dialect(vocabularyIds.map((_, true)).toMap)
      .swap
      .map(problems => throw new IllegalStateException(problems.dump()))
      .swap
      .toOption
