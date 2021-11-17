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

package frawa.typedjson.schemaSpec

import munit.FunSuite
import frawa.typedjson.schema.SchemaValue
import frawa.typedjson.parser.ZioParser
import frawa.typedjson.schema.TestUtil
import frawa.typedjson.schema.Processor
import frawa.typedjson.schema.ValidationChecker
import frawa.typedjson.schema.InnerValue
import frawa.typedjson.parser.Value
import frawa.typedjson.schema.ValidationResult
import frawa.typedjson.parser.ObjectValue
import frawa.typedjson.parser.ArrayValue
import frawa.typedjson.parser.StringValue
import frawa.typedjson.parser.BoolValue
import frawa.typedjson.schema.SpecMetaSchemas
import TestUtil._
import munit.TestOptions
import java.net.URI
import frawa.typedjson.jsonSchemaTestSuite.Remotes

class JsonSchemaTestSuiteTest extends FunSuite {
  implicit val zioParser: ZioParser = new ZioParser()

  import frawa.typedjson.macros.Macros._
  private val draft2020Files: Map[String, String] =
    folderContents("./JSON-Schema-Test-Suite/tests/draft2020-12", ".json")

  // TODO unskip 'em
  val ignore: Set[String] = Set(
    "content.json" // TODO keywords contentMediaType, contentEncoding, contentSchema
  )

  // TODO unskip 'em
  val ignoreDescription: Map[String, Set[String]] = Map(
    "dynamicRef.json" -> Set(
      "strict-tree schema, guards against misspelled properties",                     // TODO resolve URI as remote URL
      "tests for implementation dynamic anchor and reference link",                   // TODO resolve URI as remote URL
      "Tests for implementation dynamic anchor and reference link. Reference should " // TODO resolve URI as remote URL
    ),
    "vocabulary.json" -> Set(
      "schema that uses custom metaschema with with no validation vocabulary" // TODO support $schema
    ),
    "refRemote.json" -> Set(
      "base URI change - change folder" // TODO BUG!
    )
  )

  val only: Option[String] = None
  // val only: Option[String]   = Some("refRemote.json")
  val onlyId: Option[String]          = None
  val onlyDescription: Option[String] = None
  // val onlyDescription: Option[String] = Some("base URI change - change folder")

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
            ignoreDescription
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
          withStrictProcessor(ValidationChecker())(schemaValue) { processor =>
            tests.foreach(assertOne(processor))
          }
        }
      case _ => fail("invalid test json")
    }
  }

  private def assertOne(processor: Processor[ValidationResult]): Value => Unit = { value =>
    val ObjectValue(properties)  = value
    val data                     = properties("data")
    val StringValue(failMessage) = properties("description")
    val BoolValue(expected)      = properties("valid")
    val checked                  = processor(InnerValue(data))

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

  protected def checkFiles(files: Map[String, String]): Unit = {
    files
      .filterNot { case (file, _) =>
        ignore
          .contains(file)
      }
      .filter { case (file, _) =>
        only
          .map(_ == file)
          .getOrElse(true)
      }
      .foreach(check)
  }

  checkFiles(draft2020Files)

}
