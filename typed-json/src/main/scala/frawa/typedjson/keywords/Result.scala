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
  type Evalutation       = WithPointer[Evaluated]
  type OutputCombiner[O] = (O, O) => O

  def valid[O]: Result[O] = Result[O](valid = true, None, None)
  def valid[O](output: O)(implicit c: OutputCombiner[O]): Result[O] =
    Result[O](valid = true, Some(output), Some(c))
  def invalid[O]: Result[O] = Result[O](valid = false, None, None)
  def invalid[O](output: O)(implicit c: OutputCombiner[O]): Result[O] =
    Result[O](valid = false, Some(output), Some(c))

  private def count[R](results: Seq[Result[R]]): Int = results.map(_.count).sum
}

case class Result[O](
    valid: Boolean,
    output: Option[O],
    combineOutput: Option[(O, O) => O] = None,
    evaluations: Seq[Result.Evalutation] = Seq.empty,
    problems: SchemaProblems = SchemaProblems.empty,
    count: Int = 1
) {

  def addWithoutOutput(others: Seq[Result[O]]): Result[O] =
    this
      .copy(count = this.count + Result.count(others))
      .addEvaluations(others.filter(_.valid).flatMap(_.evaluations))
      .add(others.map(_.problems).reduceOption(_.combine(_)).getOrElse(SchemaProblems.empty))

  def add(other: Result[O]): Result[O] = {
    val combine = this.combineOutput.orElse(other.combineOutput)
    val os = combine.flatMap(c =>
      Seq(this.output, other.output).flatten
        .reduceOption(c)
    )
    this
      .copy(valid = this.valid && other.valid)
      .copy(count = this.count + other.count)
      .copy(output = os)
      .addEvaluations(other.evaluations)
      .add(other.problems)
  }

  def add(p: SchemaProblems): Result[O] = this.copy(problems = this.problems.combine(p))

  def add(e: Result.Evalutation): Result[O] = this.copy(evaluations = this.evaluations :+ e)

  private def addEvaluations(es: Seq[Result.Evalutation]): Result[O] =
    this.copy(evaluations = this.evaluations ++ es)

  def addIgnoredKeywords(ignored: Set[String], pointer: Pointer): Result[O] =
    if ignored.isEmpty then {
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
