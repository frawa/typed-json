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

import frawa.typedjson.parser._
import frawa.typedjson.schema.TestUtil._
import frawa.typedjson.schema._
import munit.{FunSuite, TestOptions}

import java.net.URI

class JsonSchemaTestSuite extends FunSuite {
  implicit val zioParser: ZioParser = new ZioParser()

  protected val ignore: Set[String] = Set()

  protected val ignoreDescriptionByFile: Map[String, Set[String]] = Map()
  protected type TestId = (String, String) // (file,description)
  protected val ignoreFailMessageByDescription: Map[TestId, Set[String]] = Map()

  protected val only: Option[String]            = None
  protected val onlyId: Option[String]          = None
  protected val onlyDescription: Option[String] = None

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
        val testName                 = s"${file} - ${description}"

        val useTestName = onlyDescription
          .filter(description.startsWith(_))
          .map(_ => testName.only)
          .orElse(
            ignoreDescriptionByFile
              .get(file)
              .flatMap(_.find(description.startsWith(_)))
              .map(_ => testName.ignore)
          )
          .getOrElse(new TestOptions(testName))

        test(useTestName) {
          val schema            = properties("schema")
          val ArrayValue(tests) = properties("tests")

          val id = SchemaValue.id(SchemaValue(schema))
          val includedOnlyId = onlyId
            .flatMap { onlyId =>
              id.map(_ == onlyId)
            }
          assume(includedOnlyId.getOrElse(true), s"excluded by onlyId=${onlyId}")

          val lazyResolver = (uri: URI) => SpecMetaSchemas.lazyResolver(uri).orElse(Remotes.lazyResolver(uri))
          implicit val useLazyResolver = Some(lazyResolver)
          val schemaValue              = SchemaValue(schema)
          val testId                   = (file, description)
          withStrictProcessor(ValidationChecker())(schemaValue) { processor =>
            tests.foreach(assertOne(processor, testId))
          }
        }
      case _ => fail("invalid test json")
    }
  }

  private def assertOne(processor: Processor[ValidationResult], testId: TestId): Value => Unit = { value =>
    val ObjectValue(properties)  = value
    val data                     = properties("data")
    val StringValue(failMessage) = properties("description")
    val BoolValue(expected)      = properties("valid")
    val checked                  = processor(InnerValue(data))

    val ignoreMe = ignoreFailMessageByDescription
      .get(testId)
      .exists(ignored => ignored.exists(failMessage.startsWith))
    if (ignoreMe) {
      // TODO log on test runner?
      println("IGNORING", testId, failMessage)
    } else {
      if (checked.valid != expected) {
        implicit val loc = munit.Location.empty
        if (!checked.valid) {
          assertEquals(checked.validation.errors, Seq(), failMessage)
          assertEquals(checked.validation.ignoredKeywords, Set.empty[String], failMessage)
          assertEquals(checked.results, Seq(), failMessage)
        } else {
          fail("unexpected valid", clues(clue(failMessage), clue(expected), clue(checked)))
        }
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

}
