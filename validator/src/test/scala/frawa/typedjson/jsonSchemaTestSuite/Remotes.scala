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

import java.net.URI
import frawa.typedjson.schema.LoadedSchemasResolver
import frawa.typedjson.parser.Parser
import frawa.typedjson.parser.ZioParser
import frawa.typedjson.schema.SchemaValue
import frawa.typedjson.util.UriUtil

object Remotes {

  val remotesUri: URI = UriUtil.uri("http://localhost:1234")

  def lazyResolver: LoadedSchemasResolver.LazyResolver = { uri =>
    if (uri.getSchemeSpecificPart.startsWith(remotesUri.getSchemeSpecificPart)) {
      resolveRemotes(remotesUri.relativize(uri))
    } else { None }
  }

  private def resolveRemotes(relative: URI): Option[SchemaValue] = {
    val name     = relative.getSchemeSpecificPart
    val segments = name.split('/').toArray
    if (segments.length == 2) {
      segments(0) match {
        case "baseUriChange"       => baseUriChangeFiles.get(segments(1)).flatMap(toSchemaValue)
        case "baseUriChangeFolder" => baseUriChangeFolderFiles.get(segments(1)).flatMap(toSchemaValue)
        case "baseUriChangeFolderInSubschema" =>
          baseUriChangeFolderInSubschemaFiles.get(segments(1)).flatMap(toSchemaValue)
        case _ => None
      }
    } else {
      remotesFiles.get(name).flatMap(toSchemaValue)
    }
  }

  private val parser: Parser = new ZioParser()

  private def toSchemaValue(text: String): Option[SchemaValue] = {
    parser
      .parse(text)
      .swap
      .map { e =>
        throw new IllegalArgumentException(e)
      }
      .swap
      .map(SchemaValue(_))
      .toOption
  }

  import frawa.typedjson.macros.Macros._

  private val remotesFiles: Map[String, String] =
    folderContents("./JSON-Schema-Test-Suite/remotes", ".json")

  private val baseUriChangeFiles: Map[String, String] =
    folderContents("./JSON-Schema-Test-Suite/remotes/baseUriChange", ".json")

  private val baseUriChangeFolderFiles: Map[String, String] =
    folderContents("./JSON-Schema-Test-Suite/remotes/baseUriChangeFolder", ".json")

  private val baseUriChangeFolderInSubschemaFiles: Map[String, String] =
    folderContents("./JSON-Schema-Test-Suite/remotes/baseUriChangeFolderInSubschema", ".json")

}
