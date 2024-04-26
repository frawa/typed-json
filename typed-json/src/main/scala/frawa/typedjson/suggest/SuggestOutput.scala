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

package frawa.typedjson.suggest

import frawa.typedjson.keywords.Keyword
import frawa.typedjson.keywords.KeywordLocation
import frawa.typedjson.keywords.WithLocation
import frawa.typedjson.output.OutputOps
import frawa.typedjson.output.SimpleOutput
import frawa.typedjson.pointer.Pointer
import frawa.typedjson.validation.ValidationError

import scala.collection.immutable.Seq

case class SuggestOutput(
    simple: SimpleOutput,
    keywords: Seq[Keyword] = Seq.empty
)

object SuggestOutput:
  import SimpleOutput.given

  def outputOps(at: Pointer): OutputOps[SuggestOutput] =
    SuggestOutputOps(at)

  private class SuggestOutputOps(private val at: Pointer) extends OutputOps[SuggestOutput]:
    private val bops = summon[OutputOps[SimpleOutput]]
    private val isAt = (pointer: Pointer) => at == pointer

    // TODO avoid cheating via local mutation
    import scala.collection.mutable
    private val keywordsAt = mutable.HashSet.empty[Keyword]

    def valid(pointer: Pointer): SuggestOutput =
      SuggestOutput(bops.valid(pointer), keywordsAt.toSeq)

    def invalid(error: ValidationError, pointer: Pointer): SuggestOutput =
      SuggestOutput(bops.invalid(error, pointer), keywordsAt.toSeq)

    def all(
        os: Seq[SuggestOutput],
        error: Option[ValidationError],
        pointer: Pointer
    ): SuggestOutput =
      SuggestOutput(
        bops.all(os.map(_.simple), error, pointer),
        // os.flatMap(_.keywords)
        keywordsAt.toSeq
      )

    extension (o: SuggestOutput)
      def not(pointer: Pointer): SuggestOutput = o.copy(simple = o.simple.not(pointer))
      def isValid: Boolean                     = o.simple.valid
      def withAnnotations(annotations: Seq[OutputOps.Annotation]): SuggestOutput =
        o.copy(simple = o.simple.withAnnotations(annotations))
      def getAnnotations(): Seq[OutputOps.Annotation] = o.simple.annotations
      def forKeyword(kl: KeywordLocation, k: Option[Keyword] = None): SuggestOutput =
        if isAt(o.simple.pointer) then
          k
            .map { k =>
              keywordsAt.add(k)
              // TODO avoid restoring WithLocation
              k match {
                case _: WithLocation => o.copy(keywords = o.keywords :+ k)
                case _               => o.copy(keywords = o.keywords :+ WithLocation(k, kl))
              }
            }
            .getOrElse(o)
        else o
