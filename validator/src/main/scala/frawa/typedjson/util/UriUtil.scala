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

package frawa.typedjson.util

import frawa.typedjson.pointer.Pointer

import java.net.URI

object UriUtil {

  def uri(value: String): URI = {
    URI.create(escape(value))
  }

  def withoutFragement(uri: URI): URI = new URI(uri.getScheme, uri.getSchemeSpecificPart, null)

  def withFragment(uri: URI, pointer: Pointer): URI =
    new URI(uri.getScheme, uri.getSchemeSpecificPart, escape(pointer.toString))

  def withFragment(uri: URI, fragment: String): URI =
    new URI(uri.getScheme, uri.getSchemeSpecificPart, fragment)

  case class WithLocation[+T](uri: URI, value: T)

  case class CurrentLocation(uri: URI) {
    def apply[T](value: T): WithLocation[T] = WithLocation(uri, value)
  }

  private def escape(value: String): String = {
    // this is because the ScalaJS implementation of URI might fail
    value.replace("\\", "_")
  }
}
