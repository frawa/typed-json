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

package frawa.typedjson.jsonSchemaTestSuite

import frawa.typedjson.keywords.{LoadedSchemasResolver, RootSchemaValue, SchemaValue}
import frawa.typedjson.util.UriUtil
import frawa.typedjson.foldercontents.FolderContents

import java.net.URI

object Remotes:
  import frawa.typedjson.macros.FileUtils
  import frawa.typedjson.parser.*

  val remotesUri: URI = UriUtil.uri("http://localhost:1234")

  def lazyResolver: LoadedSchemasResolver.LazyResolver = { uri =>
    if uri.getSchemeSpecificPart.startsWith(remotesUri.getSchemeSpecificPart) then
      resolveRemotes(remotesUri.relativize(uri))
    else { None }
  }

  private def resolveRemotes(relative: URI): Option[RootSchemaValue] =
    val name = relative.getSchemeSpecificPart
    remotesFiles.file(name).map(SchemaValue.root)

  import frawa.typedjson.macros.Macros.*

  private val remotesFiles: FolderContents[Value] =
    folderJsonContents("./JSON-Schema-Test-Suite/remotes", ".json")
