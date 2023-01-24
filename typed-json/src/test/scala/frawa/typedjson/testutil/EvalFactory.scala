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
import frawa.typedjson.testutil
// import frawa.typedjson.eval.Eval
// import frawa.typedjson.eval.ResultOps
// import frawa.typedjson.eval.OutputOps

trait EvalFactory[O, R[O]]:
  // given rops: ResultOps[R]
  // given ops: OutputOps[O]

  def parseKeywords(schema: SchemaValue): Either[SchemaProblems, Keywords]

  // def compile(schema: SchemaValue): Eval.EvalFun[O, R] =
  //   val keywords = parseKeywords(schema)
  //   keywords
  //     .map(Eval.compile)
  //     .fold(problems => throw new IllegalArgumentException(problems.toString()), f => f)
