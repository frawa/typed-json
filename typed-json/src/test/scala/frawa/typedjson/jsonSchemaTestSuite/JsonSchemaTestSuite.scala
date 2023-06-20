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

import frawa.inlinefiles.InlineFilesWithHome
import frawa.typedjson.eval.CacheState.R
import frawa.typedjson.eval.*
import frawa.typedjson.eval.Util.{doApply, withCompiledSchemaValue}
import frawa.typedjson.keywords.*
import frawa.typedjson.macros.Macros
import frawa.typedjson.meta.MetaSchemas
import frawa.typedjson.parser.*
import frawa.typedjson.parser.Value.*
import frawa.typedjson.pointer.Pointer
import frawa.typedjson.testutil.TestUtil.*
import munit.{FunSuite, Location, TestOptions}
import frawa.typedjson.output.OutputOps
import frawa.typedjson.output.SimpleOutput
import frawa.typedjson.output.SimpleOutput.given

import java.net.URI
import frawa.typedjson.eval.CacheState
import frawa.typedjson.eval.Util.doApplyBulk
import scala.annotation.experimental

@experimental
open class JsonSchemaTestSuite extends FunSuite:
  @experimental
  protected val draft202012 =
    InlineFilesWithHome.inlineDeepTextFiles("./JSON-Schema-Test-Suite/tests/draft2020-12", ".json")("INLINE_HOME")

  protected val ignoreFiles: Seq[String] = Seq()

  protected val ignoreByFile: Map[String, Seq[String]] = Map()

  protected type TestId = (String, String) // (file,description)
  protected val ignoreByExpectation: Map[TestId, Seq[String]] = Map()

  // test name ends with ...
  protected val onlyDescription: Option[String] = None

  private case class Expectation(description: String, data: Value, valid: Boolean)

  protected val useFormatAssertion = false

  private def vocabularyForTest = dialect(
    Seq(
      Vocabulary.coreId,
      Vocabulary.validationId,
      Vocabulary.applicatorId,
      if useFormatAssertion then Vocabulary.formatAssertionId
      else Vocabulary.formatAnnotationId,
      Vocabulary.unevaluatedId,
      Vocabulary.metaDataId
    )
  )

  protected def checkFiles[T](files: Map[String, T])(f: T => Value): Unit =
    files
      .map(t => (t._1, f(t._2)))
      .foreach { (file, suiteValue) => checkSuite(file, suiteValue) }

  private def checkSuite(file: String, testSuiteValue: Value): Unit =
    testSuiteValue match
      case ArrayValue(tests) =>
        if ignoreFiles.contains(file) then test(TestOptions(file).ignore) {}
        else tests.foreach(checkTest(file, _))
      case _ => fail("invalid test json suite")

  private def checkTest(file: String, testValue: Value): Unit =
    import SimpleOutput.given
    import CacheState.given

    testValue match
      case ObjectValue(properties) =>
        val StringValue(description: String) = properties("description"): @unchecked
        val testId                           = (file, description)
        val testName                         = s"$file - $description"
        val schema                           = properties("schema")
        val ArrayValue(testValues)           = properties("tests"): @unchecked
        val expectations                     = testValues.map(expactation)

        val isIgnoredTest = ignoreByFile.get(file).flatMap(identity).exists(description.startsWith)

        if isIgnoredTest then test(TestOptions(testName).ignore) {}
        else
          val allIgnoredExpectations = ignoreByExpectation.get(testId).flatMap(identity)
          val isIgnored: Expectation => Boolean = expectation =>
            allIgnoredExpectations.exists(expectation.description.startsWith)

          val testOptions =
            if onlyDescription.exists(testName.endsWith) then TestOptions(testName).only
            else TestOptions(testName)

          test(testOptions) {
            val schemaValue = SchemaValue.root(schema)
            expectAll(schemaValue, expectations.filterNot(isIgnored))
          }

          val ignoredExpectations = expectations.filter(isIgnored)
          ignoredExpectations.foreach { expectation =>
            val ignoredName = s"$testName | ${expectation.description}"
            test(TestOptions(ignoredName).ignore) {}
          }

      case _ => fail("invalid test json")

  private def expectAll(schemaValue: SchemaValue, expactations: Seq[Expectation]): Unit =
    @experimental
    val lazyResolver = (uri: URI) => MetaSchemas.lazyResolver(uri).orElse(Remotes.lazyResolver(uri))
    val lr           = Some(lazyResolver)
    val evalBasic    = Eval[R, SimpleOutput]

    given Eval[R, SimpleOutput]                      = evalBasic
    given Option[LoadedSchemasResolver.LazyResolver] = lr
    withCompiledSchemaValue(schemaValue, lr, vocabularyForTest) { fun =>

      val values = expactations.map(_.data)
      val os     = doApplyBulk(fun, values, { _ => })
      // val os = values.map(value => doApply(fun, value))

      given Location = munit.Location.empty
      expactations.zip(os).foreach { (expectation, o) =>
        assertEquals(
          s"'${expectation.description}'' is ${o.isValid}",
          s"'${expectation.description}'' is ${expectation.valid}",
          clues(clue[String](expectation.description), clue[Seq[SimpleOutput.Error]](o.errors))
        )
      }
    }

  private def expactation(value: Value): Expectation =
    val ObjectValue(properties)  = value: @unchecked
    val data                     = properties("data")
    val StringValue(description) = properties("description"): @unchecked
    val BoolValue(valid)         = properties("valid"): @unchecked
    Expectation(description, data, valid)
