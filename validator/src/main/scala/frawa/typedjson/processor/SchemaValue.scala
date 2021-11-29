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

import frawa.typedjson.parser.{ObjectValue, Value}
import frawa.typedjson.util.UriUtil.uri

import java.net.URI

trait SchemaValue {
  val value: Value
}
case class SchemaValue1(value: Value)                       extends SchemaValue
case class RootSchemaValue(value: Value, meta: Option[URI]) extends SchemaValue
case class MetaSchemaValue(value: Value)                    extends SchemaValue

object SchemaValue {

  def apply(value: Value): SchemaValue = SchemaValue1(value)

  def root(value: Value): RootSchemaValue = RootSchemaValue(value, get("$schema", value).map(uri))

  def id(schema: SchemaValue): Option[String] = {
    get("$id", schema.value)
  }

  private def get(property: String, value: Value): Option[String] = {
    (Pointer.empty / property)(value).flatMap(Value.asString)
  }

  def vocabulary(schema: SchemaValue, parentVocabulary: Vocabulary): Either[SchemaProblems, Vocabulary] =
    (Pointer.empty / "$vocabulary")(schema.value)
      .flatMap {
        case ObjectValue(properties) =>
          Some(properties.view.flatMap { case (k, v) => Value.asBool(v).map(v => (uri(k), v)) }.toMap)
        case _ => None
      }
      .map(Vocabulary.dialect)
      .getOrElse(Right(parentVocabulary))

}
