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

package frawa.typedjson.jsonSchemaTestSuite

import frawa.typedjson.keywords._
import frawa.typedjson.meta.MetaSchemas
import frawa.typedjson.parser.Value._
import frawa.typedjson.parser._
import frawa.typedjson.testutil.EvaluatorFactory
import frawa.typedjson.testutil.TestUtil._
import frawa.typedjson.validation.{ValidationProcessing, ValidationOutput}
import munit.{FunSuite, Location, TestOptions}

import java.net.URI

class JsonSchemaTestSuite extends FunSuite {
  protected val oneTestPerData      = false
  protected val ignore: Set[String] = Set()

  protected val ignoreDescriptionByFile: Map[String, Set[String]] = Map()
  protected type TestId = (String, String) // (file,description)
  protected val ignoreFailMessageByDescription: Map[TestId, Set[String]] = Map()

  protected val only: Option[String]            = None
  protected val onlyId: Option[String]          = None
  protected val onlyDescription: Option[String] = None

  private case class TestData(data: Value, failMessage: String, expectedValid: Boolean)

  private def check(fileAndContent: (String, Value)): Unit = {
    val (file, content) = fileAndContent
    checkSuite(file)(content)
  }

  private def checkSuite(file: String)(testSuiteValue: Value): Unit = {
    testSuiteValue match {
      case ArrayValue(tests) => tests.foreach(checkTest(file))
      case _                 => fail("invalid test json suite")
    }
  }

  private val vocabularyForTest = dialect(
    Seq(
      Vocabulary.coreId,
      Vocabulary.validationId,
      Vocabulary.applicatorId,
      Vocabulary.formatAnnotationId,
      Vocabulary.unevaluatedId,
      Vocabulary.metaDataId
    )
  )

  private def checkTest(file: String)(testValue: Value): Unit = {
    testValue match {
      case ObjectValue(properties) =>
        val StringValue(description: String) = properties("description"): @unchecked
        val suiteName                        = s"$file - $description"

        val suiteOptions = onlyDescription
          .filter(description.startsWith)
          .map(_ => suiteName.only)
          .orElse(
            ignoreDescriptionByFile
              .get(file)
              .flatMap(_.find(description.startsWith))
              .map(_ => suiteName.ignore)
          )
          .getOrElse(new TestOptions(suiteName))

        val schema            = properties("schema")
        val ArrayValue(tests) = properties("tests"): @unchecked

        val schemaValue = SchemaValue.root(schema)
        val id          = SchemaValue.id(schemaValue)
        val includedOnlyId = onlyId
          .flatMap { onlyId =>
            id.map(_ == onlyId)
          }
        assume(includedOnlyId.getOrElse(true), s"excluded by onlyId=$onlyId")

        val lazyResolver = (uri: URI) => MetaSchemas.lazyResolver(uri).orElse(Remotes.lazyResolver(uri))
        val testId       = (file, description)

        val factory: EvaluatorFactory[SchemaValue, ValidationOutput] =
          EvaluatorFactory.make(ValidationProcessing(), vocabularyForTest, lazyResolver = Some(lazyResolver))
        val strictFactory: EvaluatorFactory[SchemaValue, ValidationOutput] =
          factory.mapResult(assertNoIgnoredKeywords)

        val hasIgnoredFailMessage = ignoreFailMessageByDescription.contains(testId)
        if (oneTestPerData || hasIgnoredFailMessage) {
          implicit val factory1: EvaluatorFactory[SchemaValue, ValidationOutput] = strictFactory
          withProcessor[ValidationOutput](schemaValue) { evaluator =>
            tests.foreach { value =>
              val data     = testData(value)
              val testName = s"$file | ${data.failMessage} | $description"

              val testOptions = ignoreFailMessageByDescription
                .get(testId)
                .find(ignored => ignored.exists(data.failMessage.startsWith))
                .map(_ => testName.ignore)
                .getOrElse(new TestOptions(testName))

              test(testOptions) {
                assertOne(evaluator)(data)
              }
            }
          }
        } else {
          implicit val factory1: EvaluatorFactory[SchemaValue, ValidationOutput] = factory
          test(suiteOptions) {
            withProcessor[ValidationOutput](schemaValue) { evaluator =>
              tests
                .map(testData)
                .foreach {
                  assertOne(evaluator)
                }
            }
          }
        }
      case _ => fail("invalid test json")
    }
  }

  private def assertOne(evaluator: Evaluator[ValidationOutput]): TestData => Unit = { data =>
    val result = evaluator(InnerValue(data.data))

    if (result.valid != data.expectedValid) {
      implicit val loc: Location = munit.Location.empty
      if (!result.valid) {
        assertEquals(result.problems.errors, Seq(), data.failMessage)
        assertEquals(result.ignoredKeywords(), Set.empty[String], data.failMessage)
        assertEquals(result.output, None, data.failMessage)
      } else {
        fail(
          "unexpected valid",
          clues(clue[String](data.failMessage), clue[Boolean](data.expectedValid), clue[Result[_]](result))
        )
      }
    }
  }

  protected def checkFiles[T](files: Map[String, T])(f: T => Value): Unit = {
    files
      .filterNot { case (file, _) =>
        ignore
          .contains(file)
      }
      .filter { case (file, _) =>
        only.forall(_ == file)
      }
      .map(t => (t._1, f(t._2)))
      .foreach(check)
  }

  protected def checkFiles(files: Map[String, Value]): Unit = checkFiles[Value](files)(identity)

  private def testData(value: Value): TestData = {
    val ObjectValue(properties)  = value: @unchecked
    val data                     = properties("data")
    val StringValue(failMessage) = properties("description"): @unchecked
    val BoolValue(expected)      = properties("valid"): @unchecked
    TestData(data, failMessage, expected)
  }

}
