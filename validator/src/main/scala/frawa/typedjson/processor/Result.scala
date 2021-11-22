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

package frawa.typedjson.processor

object Result {
  type Annotation = WithPointer[Observation2]

  def apply[R](valid: Boolean, result: R): Result[R] = Result[R](valid, Seq(result))
  def valid[R]: Result[R]                            = Result[R](true)
  def valid[R](result: R): Result[R]                 = Result[R](true, Seq(result))
  def invalid[R]: Result[R]                          = Result[R](false)
  def invalid[R](result: R): Result[R]               = Result[R](false, Seq(result))

  def merge[R](allResults: Seq[Result[R]]): Result[R] = {
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
      .addValidations(others.map(_.problems))

  def add(validation: SchemaProblems): Result[R] = this.copy(problems = this.problems.combine(validation))

  private def addValidations(validations: Seq[SchemaProblems]): Result[R] =
    this.copy(problems = validations.foldLeft(this.problems)(_.combine(_)))

  def add(annotation: Result.Annotation): Result[R] = this.copy(annotations = this.annotations :+ annotation)

  private def addAnnotations(annotations: Seq[Result.Annotation]) =
    this.copy(annotations = this.annotations ++ annotations)
}
