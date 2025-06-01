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

package frawa.typedjson.schematestsuite

import frawa.typedjson.parser.Parser
import frawa.typedjson.parser.Value
import frawa.typedjson.parser.Value._
import munit.FunSuite
import munit.Location
import munit.TestOptions

import scala.collection.immutable.Seq

object SchemaTestSuite:
  type BatchValidator = Seq[Value] => Seq[(Boolean, Seq[String])]
  type TestId         = (String, String) // (file,description)
  trait Config:
    def validator(schema: Value): Either[String, BatchValidator]
    val onlyDescriptionEndsWith: Option[String]                 = None
    val ignoreFiles: Seq[String]                                = Seq()
    val ignoreByExpectationStartsWith: Map[TestId, Seq[String]] = Map()

abstract class SchemaTestSuite extends FunSuite:
  import SchemaTestSuite.*

  protected def suite(files: Map[String, String])(using Config, Parser): Unit =
    checkFiles(files)

  private def checkFiles[T](files: Map[String, String])(using Config, Parser): Unit =
    files
      .map(t => (t._1, parse(t._2)))
      .foreach { (file, suiteValue) => checkSuite(file, suiteValue) }

  private def parse(json: String)(using p: Parser): Value =
    p.parse(json)
      .fold(
        error => fail(s"invalid json file: ${error}"),
        v => v
      )

  private def checkSuite(file: String, testSuiteValue: Value)(using config: Config): Unit =
    testSuiteValue match
      case ArrayValue(tests) =>
        if config.ignoreFiles.contains(file)
        then test(TestOptions(file).ignore) {}
        else tests.foreach(checkTest(file, _))
      case _ => fail("invalid test json suite")

  private def checkTest(file: String, testValue: Value)(using config: Config): Unit =
    testValue match
      case ObjectValue(properties) =>
        val StringValue(description: String) = properties("description"): @unchecked
        val testId                           = (file, description)
        val testName                         = s"$file - $description"
        val schema                           = properties("schema")
        val ArrayValue(testValues)           = properties("tests"): @unchecked
        val expectations                     = testValues.map(expectation)

        val allIgnoredExpectations =
          config.ignoreByExpectationStartsWith.get(testId).flatMap(identity)
        val isIgnored: Expectation => Boolean = expectation =>
          allIgnoredExpectations.exists(expectation.description.startsWith)

        val testOptions =
          if config.onlyDescriptionEndsWith.exists(testName.endsWith)
          then TestOptions(testName).only
          else TestOptions(testName)

        test(testOptions) {
          expectAll(schema, expectations.filterNot(isIgnored))
        }

        val ignoredExpectations = expectations.filter(isIgnored)
        ignoredExpectations.foreach { expectation =>
          val ignoredName = s"$testName | ${expectation.description}"
          test(TestOptions(ignoredName).ignore) {}
        }

      case _ => fail("invalid test json")

  case class Expectation(description: String, data: Value, valid: Boolean)

  private def expectation(value: Value): Expectation =
    val ObjectValue(properties)  = value: @unchecked
    val data                     = properties("data")
    val StringValue(description) = properties("description"): @unchecked
    val BoolValue(valid)         = properties("valid"): @unchecked
    Expectation(description, data, valid)

  private def expectAll(schema: Value, expectations: Seq[Expectation])(using config: Config): Unit =
    val validator = config
      .validator(schema)
      .fold(error => fail(s"no validator: ${error}"), v => v)

    val values   = expectations.map(_.data)
    val actuals  = validator(values)
    val expected = expectations.map { e =>
      s"'${e.description}' is ${e.valid}"
    }
    val actual =
      expectations.zip(actuals).map { case (e, (valid, _)) =>
        s"'${e.description}' is ${valid}"
      }

    val hints = expectations
      .zip(actuals)
      .map { case (e, (valid, errors)) =>
        if e.valid != valid
        then
          val withErrors =
            if errors.nonEmpty
            then ": " + errors.mkString(",")
            else ""
          Some(s"${e.description}${withErrors}")
        else None
      }
      .flatten
    assertEquals(actual, expected, clue(hints))
