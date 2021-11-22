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

import frawa.typedjson.meta.MetaSchemas
import frawa.typedjson.parser.{BoolValue, ObjectValue, StringValue, _}
import frawa.typedjson.testutil.TestUtil._
import frawa.typedjson.processor._
import frawa.typedjson.testutil.TestUtil
import frawa.typedjson.validation.{ValidationEval, ValidationResult}
import munit.{FunSuite, TestOptions}

import java.net.URI

class JsonSchemaTestSuite extends FunSuite {
  implicit val zioParser: ZioParser = new ZioParser()

  protected val oneTestPerData      = false
  protected val ignore: Set[String] = Set()

  protected val ignoreDescriptionByFile: Map[String, Set[String]] = Map()
  protected type TestId = (String, String) // (file,description)
  protected val ignoreFailMessageByDescription: Map[TestId, Set[String]] = Map()

  protected val only: Option[String]            = None
  protected val onlyId: Option[String]          = None
  protected val onlyDescription: Option[String] = None

  private case class TestData(data: Value, failMessage: String, expectedValid: Boolean)

  private def check(fileAndContent: (String, String)): Unit = {
    val (file, content) = fileAndContent
    checkSuite(file)(TestUtil.parseJsonValue(content))
  }

  private def checkSuite(file: String)(testSuiteValue: Value): Unit = {
    testSuiteValue match {
      case ArrayValue(tests) => tests.foreach(checkTest(file))
      case _                 => fail("invalid test json suite")
    }
  }

  private def checkTest(file: String)(testValue: Value): Unit = {
    testValue match {
      case ObjectValue(properties) =>
        val StringValue(description) = properties("description")
        val suiteName                = s"${file} - ${description}"

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
        val ArrayValue(tests) = properties("tests")

        val id = SchemaValue.id(SchemaValue(schema))
        val includedOnlyId = onlyId
          .flatMap { onlyId =>
            id.map(_ == onlyId)
          }
        assume(includedOnlyId.getOrElse(true), s"excluded by onlyId=${onlyId}")

        val lazyResolver             = (uri: URI) => MetaSchemas.lazyResolver(uri).orElse(Remotes.lazyResolver(uri))
        implicit val useLazyResolver = Some(lazyResolver)
        val schemaValue              = SchemaValue(schema)
        val testId                   = (file, description)

        val hasIgnoredFailMessage = ignoreFailMessageByDescription.contains(testId)
        if (oneTestPerData || hasIgnoredFailMessage) {
          withStrictProcessor(ValidationEval())(schemaValue) { processor =>
            tests.foreach { value =>
              val data     = testData(value)
              val testName = s"${file} | ${data.failMessage} | ${description}"

              val testOptions = ignoreFailMessageByDescription
                .get(testId)
                .find(ignored => ignored.exists(data.failMessage.startsWith))
                .map(_ => testName.ignore)
                .getOrElse(new TestOptions(testName))

              test(testOptions) {
                assertOne(processor)(data)
              }
            }
          }
        } else {
          test(suiteOptions) {
            withStrictProcessor(ValidationEval())(schemaValue) { processor =>
              tests
                .map(testData)
                .foreach {
                  assertOne(processor)
                }
            }
          }
        }
      case _ => fail("invalid test json")
    }
  }

  private def assertOne(processor: Processor[ValidationResult]): TestData => Unit = { data =>
    val result = processor(InnerValue(data.data))

    if (result.valid != data.expectedValid) {
      implicit val loc = munit.Location.empty
      if (!result.valid) {
        assertEquals(result.problems.errors, Seq(), data.failMessage)
        assertEquals(result.problems.ignoredKeywords, Set.empty[String], data.failMessage)
        assertEquals(result.results, Seq(), data.failMessage)
      } else {
        fail("unexpected valid", clues(clue(data.failMessage), clue(data.expectedValid), clue(result)))
      }
    }
  }

  protected def checkFiles(files: Map[String, String]): Unit = {
    files
      .filterNot { case (file, _) =>
        ignore
          .contains(file)
      }
      .filter { case (file, _) =>
        only.forall(_ == file)
      }
      .foreach(check)
  }

  private def testData(value: Value): TestData = {
    val ObjectValue(properties)  = value
    val data                     = properties("data")
    val StringValue(failMessage) = properties("description")
    val BoolValue(expected)      = properties("valid")
    TestData(data, failMessage, expected)
  }

}
