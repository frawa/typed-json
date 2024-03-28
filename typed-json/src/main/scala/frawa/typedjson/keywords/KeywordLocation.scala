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
import frawa.typedjson.util.UriUtil

import java.net.URI

enum KeywordLocation:
  case Local(relative: Pointer)
  case Dereferenced(relative: Pointer, absolute: URI)

  def push(pushFun: Pointer => Pointer): KeywordLocation = this match {
    case Local(relative) => Local(pushFun(relative))
    case Dereferenced(relative, absolute) =>
      Dereferenced(pushFun(relative), UriUtil.push(absolute, pushFun))
  }

  def resolved(base: URI): KeywordLocation = this match {
    case Local(relative)                  => Dereferenced(relative, base)
    case Dereferenced(relative, absolute) => Dereferenced(relative, base)
  }

  def parent: KeywordLocation = this match {
    case Local(relative) => Local(relative.outer)
    case Dereferenced(relative, absolute) =>
      Dereferenced(relative.outer, UriUtil.push(absolute, _.outer))
  }

object KeywordLocation:
  def empty: KeywordLocation =
    Local(Pointer.empty)

  def apply(relative: String, absolute: String): KeywordLocation =
    Dereferenced(Pointer.parse(relative).getOrElse(Pointer.empty), UriUtil.uri(absolute))

  def apply(relative: String): KeywordLocation =
    apply(Pointer.parse(relative).getOrElse(Pointer.empty))

  def apply(relative: Pointer): KeywordLocation =
    Local(relative)
