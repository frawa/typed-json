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

case class SchemaProblems(
    errors: Seq[SchemaProblems.SchemaError]
) {
  def addErrors(errors: Seq[SchemaProblems.SchemaError]): SchemaProblems =
    copy(errors = this.errors ++ errors)

  def combine(other: SchemaProblems): SchemaProblems =
    copy(errors = this.errors ++ other.errors)
}

object SchemaProblems {
  val empty: SchemaProblems = SchemaProblems(Seq.empty)

  trait Error
  case class InvalidSchemaValue(schema: Value)    extends Error
  case class MissingReference(ref: String)        extends Error
  case class MissingDynamicReference(ref: String) extends Error

  type SchemaError = WithPointer[Error]
}
