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

package frawa.typedjson.meta

import frawa.typedjson.keywords.{LoadedSchemasResolver, RootSchemaValue, SchemaValue}
import frawa.typedjson.macros.Macros
import frawa.typedjson.util.UriUtil.uri

import java.net.URI

object MetaSchemas:

  val draft202012: URI = uri("https://json-schema.org/draft/2020-12/")

  def lazyResolver: LoadedSchemasResolver.LazyResolver = { uri =>
    if uri.getSchemeSpecificPart.startsWith(draft202012.getSchemeSpecificPart) then
      resolve202012(draft202012.relativize(uri))
    else { None }
  }

  private def resolve202012(relative: URI): Option[RootSchemaValue] =
    val name = relative.getSchemeSpecificPart
    metaSchemas.file(name + ".json").map(SchemaValue.root(_))

  import Macros.*

  private val metaSchemas = folderJsonContents("./metaSchemas", ".json")
