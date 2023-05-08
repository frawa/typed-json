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

package frawa.typedjson.output

import frawa.typedjson.util.WithPointer
import frawa.typedjson.validation.ValidationError
import frawa.typedjson.validation.ValidationAnnotation
import frawa.typedjson.pointer.Pointer
import frawa.typedjson.keywords.SchemaProblems
import frawa.typedjson.validation.NotOneOf
import frawa.typedjson.validation.NotInvalid
import frawa.typedjson.keywords.Evaluated
import frawa.typedjson.keywords.Keyword
import frawa.typedjson.output.OutputOps
import frawa.typedjson.keywords.KeywordLocation
import java.awt.event.KeyListener
import frawa.typedjson.output.BasicOutput

// TODO this will converge to "basic" output format,
// see https://json-schema.org/draft/2020-12/json-schema-core.html#name-basic

case class BasicOutput(
    valid: Boolean,
    annotations: Seq[Evaluated] = Seq.empty,
    error: Option[ValidationError] = None,
    errors: Seq[BasicOutput.Error] = Seq(),
    // subs: Seq[BasicOutput] = Seq(),
    instanceLocation: Pointer = Pointer.empty,
    keywordLocation: Option[KeywordLocation] = None
)

object BasicOutput:
  case class Error(
      error: ValidationError,
      instanceLocation: Pointer = Pointer.empty,
      keywordLocation: KeywordLocation = KeywordLocation.empty
  )

  given OutputOps[BasicOutput] with
    def valid(pointer: Pointer): BasicOutput = BasicOutput(true, instanceLocation = pointer)
    def invalid(error: ValidationError, pointer: Pointer): BasicOutput =
      BasicOutput(false, error = Some(error), instanceLocation = pointer)

    def all(os: Seq[BasicOutput], pointer: Pointer): BasicOutput =
      val valid = os.forall(_.valid)
      val annotations =
        if valid then OutputOps.mergeAnnotations(os.filter(_.instanceLocation == pointer).flatMap(_.annotations))
        else Seq()
      val errors = os.flatMap { o =>
        val self = o.error.map { error =>
          Error(error, o.instanceLocation, o.keywordLocation.getOrElse(KeywordLocation.empty))
        }.toSeq
        self ++ o.errors
      }
      BasicOutput(
        valid,
        annotations,
        errors = errors,
        instanceLocation = pointer
      )

    extension (o: BasicOutput)
      def not(pointer: Pointer): BasicOutput =
        if o.valid then BasicOutput(valid = false, error = Some(NotInvalid()), instanceLocation = pointer)
        else BasicOutput(valid = true, instanceLocation = pointer)
      def isValid: Boolean                                          = o.valid
      def withAnnotations(annotations: Seq[Evaluated]): BasicOutput = o.copy(annotations = o.annotations ++ annotations)
      def getAnnotations(): Seq[Evaluated]                          = o.annotations
      def forKeyword(k: Keyword, kl: Option[KeywordLocation]): BasicOutput =
        // if (kl.isEmpty) then
        // new RuntimeException("?").printStackTrace()
        // if (kl.isDefined && o.keywordLocation.isDefined) then new RuntimeException("??").printStackTrace()
        // TODO avoid!
        if (kl.isEmpty && o.keywordLocation.isDefined) then o
        // new RuntimeException("???").printStackTrace()
        else o.copy(keywordLocation = kl)
