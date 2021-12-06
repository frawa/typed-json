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

sealed trait Evaluated
case class EvaluatedIndices(indices: Seq[Int])          extends Evaluated
case class EvaluatedProperties(properties: Set[String]) extends Evaluated
case class Ignored(keywords: Set[String])               extends Evaluated

object Result {
  type Annotation = WithPointer[Evaluated]

  def apply[R](valid: Boolean, result: R): Result[R] = Result[R](valid, Seq(result))
  def valid[R]: Result[R]                            = Result[R](valid = true)
  def valid[R](result: R): Result[R]                 = Result[R](valid = true, Seq(result))
  def invalid[R]: Result[R]                          = Result[R](valid = false)
  def invalid[R](result: R): Result[R]               = Result[R](valid = false, Seq(result))

  def combine[R](allResults: Seq[Result[R]]): Result[R] = {
    val valid       = allResults.forall(_.valid)
    val results     = allResults.flatMap(_.results)
    val problems    = allResults.map(_.problems).reduceOption(_.combine(_)).getOrElse(SchemaProblems.empty)
    val annotations = allResults.filter(_.valid).flatMap(_.annotations)
    Result(valid, results, annotations, problems, 1 + count(allResults))
  }

  def count[R](results: Seq[Result[R]]): Int = results.map(_.count).sum
}

case class Result[R](
    valid: Boolean,
    results: Seq[R] = Seq.empty,
    annotations: Seq[Result.Annotation] = Seq.empty,
    problems: SchemaProblems = SchemaProblems.empty,
    count: Int = 1
) {
  def add(others: Seq[Result[R]]): Result[R] =
    this
      .copy(count = this.count + Result.count(others))
      .addAnnotations(others.flatMap(_.annotations))
      .addProblems(others.map(_.problems))

  def add(problem: SchemaProblems): Result[R] = this.copy(problems = this.problems.combine(problem))

  private def addProblems(problems: Seq[SchemaProblems]): Result[R] =
    this.copy(problems = problems.foldLeft(this.problems)(_.combine(_)))

  def add(annotation: Result.Annotation): Result[R] = this.copy(annotations = this.annotations :+ annotation)

  private def addAnnotations(annotations: Seq[Result.Annotation]): Result[R] =
    this.copy(annotations = this.annotations ++ annotations)

  def addIgnoredKeywords(ignored: Set[String], pointer: Pointer): Result[R] =
    if (ignored.isEmpty) {
      this
    } else {
      add(WithPointer(Ignored(ignored), pointer))
    }

  def ignoredKeywords(): Set[String] = {
    annotations
      .flatMap {
        case WithPointer(Ignored(ignored), _) => Some(ignored)
        case _                                => None
      }
      .reduceOption(_ ++ _)
      .getOrElse(Set.empty)
  }

}
