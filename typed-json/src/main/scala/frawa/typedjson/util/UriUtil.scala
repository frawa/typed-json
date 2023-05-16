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
import frawa.typedjson.keywords.KeywordLocation

object UriUtil:

  def uri(value: String): URI =
    URI.create(escape(value))

  def absolute(ref: String, base: URI): URI =
    val uri1 = uri(ref)
    withoutEmptyFragment(
      Some(uri1)
        .filter(_.isAbsolute())
        .getOrElse(UriUtil.resolve(base, uri1))
    )

  private def withoutEmptyFragment(uri: URI): URI =
    val fragment = uri.getFragment
    if fragment != null && fragment.isEmpty then withoutFragement(uri)
    else uri

  def withoutFragement(uri: URI): URI = new URI(uri.getScheme, uri.getSchemeSpecificPart, null)

  def withFragment(uri: URI, pointer: Pointer): URI =
    new URI(uri.getScheme, uri.getSchemeSpecificPart, escape(pointer.toString))

  def withFragment(uri: URI, fragment: String): URI =
    new URI(uri.getScheme, uri.getSchemeSpecificPart, fragment)

  def resolve(base: URI, uri: URI): URI =
    if base.isOpaque() && isFragment(uri) then withFragment(base, uri.getFragment())
    else base.resolve(uri)

  private def isFragment(uri: URI): Boolean =
    !uri.isOpaque() && uri.getScheme() == null && uri.getAuthority() == null && uri.getPath().isEmpty &&
      uri.getFragment() != null

  case class CurrentLocation(kl: KeywordLocation)

  private def escape(value: String): String =
    // this is because the ScalaJS implementation of URI might fail
    value.replace("\\", "_")

  def push(uri: URI, pushFun: Pointer => Pointer): URI =
    val pushedPointer = Pointer.parse(uri.getFragment).map(pushFun).getOrElse(Pointer.empty)
    withFragment(uri, pushedPointer)
