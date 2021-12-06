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

import frawa.typedjson.processor.{LoadedSchemasResolver, RootSchemaValue, SchemaValue}
import frawa.typedjson.util.UriUtil

import java.net.URI

object Remotes {
  import frawa.typedjson.parser._

  val remotesUri: URI = UriUtil.uri("http://localhost:1234")

  def lazyResolver: LoadedSchemasResolver.LazyResolver = { uri =>
    if (uri.getSchemeSpecificPart.startsWith(remotesUri.getSchemeSpecificPart)) {
      resolveRemotes(remotesUri.relativize(uri))
    } else { None }
  }

  private def resolveRemotes(relative: URI): Option[RootSchemaValue] = {
    val name     = relative.getSchemeSpecificPart
    val segments = name.split('/')
    if (segments.length == 2) {
      segments(0) match {
        case "baseUriChange"       => baseUriChangeFiles.get(segments(1)).map(SchemaValue.root)
        case "baseUriChangeFolder" => baseUriChangeFolderFiles.get(segments(1)).map(SchemaValue.root)
        case "baseUriChangeFolderInSubschema" =>
          baseUriChangeFolderInSubschemaFiles.get(segments(1)).map(SchemaValue.root)
        case "draft2020-12" => draft202012Files.get(segments(1)).map(SchemaValue.root)
        case _ =>
          println("MISSING in Remotes", segments(0))
          None
      }
    } else {
      remotesFiles.get(name).map(SchemaValue.root)
    }
  }

  import frawa.typedjson.macros.Macros._

  private val remotesFiles: Map[String, Value] =
    folderJsonContents("./JSON-Schema-Test-Suite/remotes", ".json")

  private val baseUriChangeFiles: Map[String, Value] =
    folderJsonContents("./JSON-Schema-Test-Suite/remotes/baseUriChange", ".json")

  private val baseUriChangeFolderFiles: Map[String, Value] =
    folderJsonContents("./JSON-Schema-Test-Suite/remotes/baseUriChangeFolder", ".json")

  private val baseUriChangeFolderInSubschemaFiles: Map[String, Value] =
    folderJsonContents("./JSON-Schema-Test-Suite/remotes/baseUriChangeFolderInSubschema", ".json")

  private val draft202012Files: Map[String, Value] =
    folderJsonContents("./JSON-Schema-Test-Suite/remotes/draft2020-12", ".json")
}
