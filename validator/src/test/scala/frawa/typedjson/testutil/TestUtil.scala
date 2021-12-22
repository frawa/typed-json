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

import frawa.typedjson.jawn.JawnParser
import frawa.typedjson.keywords._
import frawa.typedjson.parser.{Parser, Value}
import munit.Assertions.{assertEquals, clue, clues, fail}

import java.net.URI

object TestUtil {
  implicit val parser: JawnParser                                       = new JawnParser
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
    f(SchemaValue.root(parseJsonValue(text)))
  }

  def withLoadedSchemas(texts: Seq[String])(f: LoadedSchemasResolver => Unit)(implicit parser: Parser): Unit = {
    val schemas  = texts.map(t => parseJsonValue(t)).map(SchemaValue(_))
    val resolver = LoadedSchemasResolver(schemas)
    f(resolver)
  }

  def assertNoIgnoredKeywords[R]: Result[R] => Result[R] = { result =>
    assertEquals(result.ignoredKeywords(), Set.empty[String], "ignored keywords")
    result
  }

  def assertResult[R](valueText: String)(schema: SchemaValue)(
      f: Result[R] => Unit
  )(implicit factory: EvaluatorFactory[SchemaValue, R], parser: Parser): Either[Nothing, Unit] = {
    withProcessor[R](schema) { evaluator =>
      val value  = parseJsonValue(valueText)
      val result = evaluator(InnerValue(value))
      f(result)
    }
  }

  def withProcessor[R](schema: SchemaValue)(
      f: Evaluator[R] => Unit
  )(implicit factory: EvaluatorFactory[SchemaValue, R]): Either[Nothing, Unit] = {
    val result = factory(schema).map(f)
    result.swap
      .map(message => fail("creating keywords failed", clues(clue(message))))
      .swap
  }

  def dialect(vocabularyIds: Seq[URI]): Option[Vocabulary] = {
    Vocabulary
      .dialect(vocabularyIds.map((_, true)).toMap)
      .swap
      .map(problems => throw new IllegalStateException(problems.dump()))
      .swap
      .toOption
  }

  def assertable(keywords: Keywords): Keywords = keywords.copy(keywords = keywords.keywords.map(assertable))

  val assertableResolve: () => Either[SchemaProblems, Keywords] = () => Left(SchemaProblems.empty)

  private def assertable(keyword: Keywords.KeywordWithLocation): Keywords.KeywordWithLocation =
    keyword.copy(value = assertable(keyword.value))

  private def assertable(keyword: Keyword): Keyword = keyword match {
    case ArrayItemsKeyword(items, prefixItems) =>
      ArrayItemsKeyword(
        items.map(assertable),
        prefixItems.map(assertable)
      )
    case LazyParseKeywords(resolved, _) => LazyParseKeywords(resolved, assertableResolve)
    case _                              => keyword
  }

}
