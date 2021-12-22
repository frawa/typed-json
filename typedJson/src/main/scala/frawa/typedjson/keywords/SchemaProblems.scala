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

import frawa.typedjson.parser.Value
import frawa.typedjson.pointer.Pointer

import java.net.URI

case class SchemaProblems(
    errors: Seq[SchemaProblems.SchemaError]
) {

  def addErrors(errors: Seq[SchemaProblems.SchemaError]): SchemaProblems =
    copy(errors = this.errors ++ errors)

  def prefix(pointer: Pointer): SchemaProblems =
    copy(errors = this.errors.map(_.prefix(pointer)))

  def combine(other: SchemaProblems): SchemaProblems =
    copy(errors = this.errors ++ other.errors)

  def dump(): String = errors.map(_.toString).mkString("\n")

}

object SchemaProblems {
  val empty: SchemaProblems = SchemaProblems(Seq.empty)

  def apply(error: Error): SchemaProblems = SchemaProblems(Seq(WithPointer(error)))

  trait Error
  case class InvalidSchemaValue(schema: Value)    extends Error
  case class MissingReference(ref: String)        extends Error
  case class MissingDynamicReference(ref: String) extends Error
  case class UnknownRequiredVocabulary(id: URI)   extends Error
  case class UnsupportedKeyword(keyword: String)  extends Error

  type SchemaError = WithPointer[Error]

  def combine(a: SchemaProblems, b: SchemaProblems): SchemaProblems = a.combine(b)
}
