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

import frawa.typedjson.parser.Value

case class SchemaQuality(
    errors: Seq[SchemaQuality.SchemaError],
    ignoredKeywords: Set[String],
    pointer: Pointer = Pointer.empty
) {
  def addIgnoredKeywords(ignoredKeywords: Set[String]): SchemaQuality =
    copy(ignoredKeywords = this.ignoredKeywords ++ ignoredKeywords)

  def addErrors(errors: Seq[SchemaQuality.SchemaError]): SchemaQuality =
    copy(errors = this.errors ++ errors)

  def combine(other: SchemaQuality): SchemaQuality =
    copy(errors = this.errors ++ other.errors, ignoredKeywords = this.ignoredKeywords ++ other.ignoredKeywords)
}

object SchemaQuality {
  val empty: SchemaQuality = SchemaQuality(Seq.empty, Set.empty)

  trait Error
  case class InvalidSchemaValue(schema: Value)    extends Error
  case class MissingReference(ref: String)        extends Error
  case class MissingDynamicReference(ref: String) extends Error

  type SchemaError = WithPointer[Error]
}
