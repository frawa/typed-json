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

package frawa.typedjson.testsuite
import frawa.typedjson.eval.CacheState.R
import frawa.typedjson.eval.Util.doApplyBulk
import frawa.typedjson.eval.Util.withCompiledSchemaValue
import frawa.typedjson.eval._
import frawa.typedjson.keywords._
import frawa.typedjson.meta.MetaSchemas
import frawa.typedjson.output.SimpleOutput
import frawa.typedjson.output.SimpleOutput.given
import frawa.typedjson.parser._
import frawa.typedjson.schematestsuite.SchemaTestSuite.BatchValidator
import frawa.typedjson.schematestsuite.SchemaTestSuite.Config
import frawa.typedjson.schematestsuite.SchemaTestSuite.TestId
import frawa.typedjson.testutil.TestUtil._

import java.net.URI
import scala.collection.immutable.Seq

case class TestConfig(
    override val ignoreFiles: Seq[String] = Seq(),
    override val ignoreByExpectationStartsWith: Map[TestId, Seq[String]] = Map()
)(using Parser)
    extends Config:
  def validator(schema: Value): Either[String, BatchValidator] =
    val lazyResolver = (uri: URI) => MetaSchemas.lazyResolver(uri).orElse(Remotes.lazyResolver(uri))
    val lr           = Some(lazyResolver)
    val evalBasic    = Eval[R, SimpleOutput]
    given Eval[R, SimpleOutput] = evalBasic
    val schemaValue             = SchemaValue.root(schema)
    Right(values =>
      withCompiledSchemaValue(schemaValue, lr, vocabularyForTest(false)) { fun =>
        doApplyBulk(fun, values, _ => ())
          .map(o => (o.valid, o.errors.map(_.toString)))
      }
    )

private def vocabularyForTest(useFormatAssertion: Boolean) = dialect(
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
