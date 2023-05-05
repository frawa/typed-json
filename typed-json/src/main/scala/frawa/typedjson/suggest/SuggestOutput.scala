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

import frawa.typedjson.util.WithPointer
import frawa.typedjson.validation.ValidationError
import frawa.typedjson.validation.ValidationAnnotation
import frawa.typedjson.pointer.Pointer
import frawa.typedjson.keywords.SchemaProblems
import frawa.typedjson.validation.NotOneOf
import frawa.typedjson.validation.NotInvalid
import frawa.typedjson.keywords.Evaluated
import frawa.typedjson.keywords.Keyword
import frawa.typedjson.output.SimpleOutput
import frawa.typedjson.output.OutputOps
import frawa.typedjson.suggest.Suggest
import frawa.typedjson.keywords.KeywordLocation

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
    private val isAt = Suggest.isAt(at)

    def valid(pointer: Pointer): SuggestOutput =
      SuggestOutput(bops.valid(pointer))

    def invalid(error: ValidationError, pointer: Pointer): SuggestOutput =
      SuggestOutput(bops.invalid(error, pointer))

    def all(os: Seq[SuggestOutput], pointer: Pointer): SuggestOutput =
      SuggestOutput(
        bops.all(os.map(_.simple), pointer),
        os.flatMap(_.keywords)
      )

    extension (o: SuggestOutput)
      def not(pointer: Pointer): SuggestOutput = o.copy(simple = o.simple.not(pointer))
      def isValid: Boolean                     = o.simple.valid
      def withAnnotations(annotations: Seq[Evaluated]): SuggestOutput =
        o.copy(simple = o.simple.withAnnotations(annotations))
      def getAnnotations(): Seq[Evaluated] = o.simple.annotations
      def forKeyword(k: Keyword, kl: Option[KeywordLocation]): SuggestOutput =
        if isAt(o.simple.pointer) then o.copy(keywords = o.keywords :+ k)
        else o
