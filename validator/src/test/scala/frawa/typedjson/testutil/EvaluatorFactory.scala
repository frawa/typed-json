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

import frawa.typedjson.keywords._
import frawa.typedjson.testutil

case class EvaluatorFactory[V, R](create: EvaluatorFactory.CreateFun[V, R]) {

  def apply(v: V): Either[SchemaProblems, Evaluator[R]] = create(v)

  def mapResult(f: Result[R] => Result[R]): EvaluatorFactory[V, R] = {
    EvaluatorFactory(create.andThen(_.map { evaluator =>
      evaluator.andThen(f)
    }))
  }
}

object EvaluatorFactory {
  type CreateFun[V, R] = V => Either[SchemaProblems, Evaluator[R]]

  def make[R](
      processing: Processing[R],
      vocabulary: Option[Vocabulary] = None,
      lazyResolver: Option[LoadedSchemasResolver.LazyResolver] = None
  ): EvaluatorFactory[SchemaValue, R] = testutil.EvaluatorFactory({ schema =>
    Keywords(schema, vocabulary, lazyResolver).map(Evaluator(_, processing))
  })

}
