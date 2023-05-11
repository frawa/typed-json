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

import frawa.typedjson.pointer.Pointer

import java.net.URI

case class DynamicScope(uris: Seq[URI], kl: KeywordLocation = KeywordLocation.empty):

  import frawa.typedjson.util.UriUtil.*

  def candidates: Seq[URI] = uris
    .map(withoutFragement)
    .distinct

  def push(segment: String): DynamicScope =
    push(_ / segment)

  def push(index: Int): DynamicScope =
    push(_ / index)

  def push(next: URI): DynamicScope =
    this.copy(uris = uris :+ next)

  def resolved(base: URI): DynamicScope =
    copy(kl = kl.resolved(base))

  private def push(pushFun: Pointer => Pointer): DynamicScope =
    val uri1 = uris.lastOption.getOrElse(uri(""))
    val pointer = uris.lastOption
      .map(_.getFragment())
      .filter(_ != null)
      .map(Pointer.parse(_))
      .getOrElse(Pointer.empty)
    val pushPointer = pushFun(pointer)
    val next        = withFragment(uri1, pushPointer)
    push(next).copy(kl = kl.push(pushFun))

  def currentLocation: CurrentLocation =
    CurrentLocation(kl)

object DynamicScope:
  def empty: DynamicScope = DynamicScope(Seq())
