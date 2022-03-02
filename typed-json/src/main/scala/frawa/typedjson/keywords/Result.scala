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

package frawa.typedjson.keywords

import frawa.typedjson.pointer.Pointer

sealed trait Evaluated
case class EvaluatedIndices(indices: Seq[Int])          extends Evaluated
case class EvaluatedProperties(properties: Set[String]) extends Evaluated
case class Ignored(keywords: Set[String])               extends Evaluated

object Result {
  type Evalutation = WithPointer[Evaluated]

  def apply[R](valid: Boolean, result: R): Result[R] = Result[R](valid, Seq(result))
  def valid[R]: Result[R]                            = Result[R](valid = true)
  def valid[R](result: R): Result[R]                 = Result[R](valid = true, Seq(result))
  def invalid[R]: Result[R]                          = Result[R](valid = false)
  def invalid[R](result: R): Result[R]               = Result[R](valid = false, Seq(result))

  def combine[R](allResults: Seq[Result[R]]): Result[R] = {
    val valid        = allResults.forall(_.valid)
    val results      = allResults.flatMap(_.results)
    val problems     = allResults.map(_.problems).reduceOption(_.combine(_)).getOrElse(SchemaProblems.empty)
    val evalutations = allResults.filter(_.valid).flatMap(_.evaluations)
    Result(valid, results, evalutations, problems, 1 + count(allResults))
  }

  def count[R](results: Seq[Result[R]]): Int = results.map(_.count).sum
}

case class Result[R](
    valid: Boolean,
    results: Seq[R] = Seq.empty,
    evaluations: Seq[Result.Evalutation] = Seq.empty,
    problems: SchemaProblems = SchemaProblems.empty,
    count: Int = 1
) {
  def add(others: Seq[Result[R]]): Result[R] =
    this
      .copy(count = this.count + Result.count(others))
      .addEvaluations(others.flatMap(_.evaluations))
      .addProblems(others.map(_.problems))

  def add(problem: SchemaProblems): Result[R] = this.copy(problems = this.problems.combine(problem))

  private def addProblems(problems: Seq[SchemaProblems]): Result[R] =
    this.copy(problems = problems.foldLeft(this.problems)(_.combine(_)))

  def add(evaluation: Result.Evalutation): Result[R] = this.copy(evaluations = this.evaluations :+ evaluation)

  private def addEvaluations(evaluations: Seq[Result.Evalutation]): Result[R] =
    this.copy(evaluations = this.evaluations ++ evaluations)

  def addIgnoredKeywords(ignored: Set[String], pointer: Pointer): Result[R] =
    if (ignored.isEmpty) {
      this
    } else {
      add(WithPointer(Ignored(ignored), pointer))
    }

  def ignoredKeywords(): Set[String] = {
    evaluations
      .flatMap {
        case WithPointer(Ignored(ignored), _) => Some(ignored)
        case _                                => None
      }
      .reduceOption(_ ++ _)
      .getOrElse(Set.empty)
  }
}
