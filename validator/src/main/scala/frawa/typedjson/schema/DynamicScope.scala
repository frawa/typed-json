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

package frawa.typedjson.schema

import frawa.typedjson.parser.Value
import frawa.typedjson.parser.StringValue
import java.net.URI

case class DynamicScope(uris: Seq[URI]) {
  import DynamicScope._
  import UriUtil._

  def candidates: Seq[URI] = uris
    .map(withoutFragement)
    .distinct

  def push(segment: String): DynamicScope = {
    val uri = uris.lastOption.getOrElse(URI.create(""))
    val pointer = uris.lastOption
      .map(_.getFragment())
      .filter(_ != null)
      .map(Pointer.parse(_))
      .getOrElse(Pointer.empty)
    val pushPointer = pointer / segment
    val next        = withFragment(uri, pushPointer)
    DynamicScope(uris :+ next)
  }

  def push(next: URI): DynamicScope = {
    DynamicScope(uris :+ next)
  }
}

object DynamicScope {
  def empty: DynamicScope = DynamicScope(Seq())
}