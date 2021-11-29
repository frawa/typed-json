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

import frawa.typedjson.util.UriUtil
import frawa.typedjson.util.UriUtil._

import java.net.URI

object SchemaResolver {
  type Resolution = (SchemaValue, SchemaResolver)
}

trait SchemaResolver {
  type Resolution = SchemaResolver.Resolution

  val base: URI

  protected def resolve(uri: URI): Option[Resolution] = None
  protected def isDynamic(uri: URI): Boolean          = false

  def withBase(uri: URI): SchemaResolver = this

  def absolute(ref: String): URI = {
    val uri1 = uri(ref)
    withoutEmptyFragment(
      Some(uri1)
        .filter(_.isAbsolute())
        .getOrElse(base.resolve(uri1))
    )
  }

  def resolveDynamicRef(ref: String, scope: DynamicScope): Option[Resolution] = {
    resolveDynamicRef(absolute(ref), scope)
  }

  def resolveRef(ref: String): Option[Resolution] = {
    resolveRef(absolute(ref))
  }

  private def resolveDynamicRef(uri: URI, scope: DynamicScope): Option[Resolution] = {
    val resolved = resolveRef(uri)

    val fragment = uri.getFragment
    val dynamic = isDynamic(uri) ||
      scope.candidates.lastOption.map(UriUtil.withFragment(_, fragment)).exists(isDynamic)
    if (dynamic && fragment != null) {
      scope.candidates
        .map(UriUtil.withFragment(_, fragment))
        .find(isDynamic)
        .flatMap(resolve)
        .orElse(resolved)
    } else {
      resolved
    }
  }

  private def withoutEmptyFragment(uri: URI): URI = {
    val fragment = uri.getFragment()
    if (fragment != null && fragment.isEmpty()) {
      UriUtil.withoutFragement(uri)
    } else {
      uri
    }
  }

  def resolveRef(uri: URI): Option[Resolution] = {
    if (uri.getFragment != null && uri.getFragment.startsWith("/")) {
      val pointer = Pointer.parse(uri.getFragment)
      resolve(UriUtil.withoutFragement(uri))
        .flatMap(resolvePointer(_, pointer))
    } else {
      resolve(uri)
    }
  }

  private def resolvePointer(resolution: Resolution, pointer: Pointer): Option[Resolution] = {
    val (schema, resolver) = resolution
    pointer(schema.value).map(SchemaValue(_)).map((_, resolver))
  }
}
