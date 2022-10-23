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

package frawa.typedjson.macros

import frawa.typedjson.parser.Value

object Macros:
  import scala.quoted.*
  import FileUtils._
  import JsonUtils._

  inline def fileContent(inline path: String): String = ${
    fileContent_impl('path)
  }

  private def fileContent_impl(path: Expr[String])(using Quotes): Expr[String] =
    Expr(readContentOf(path.valueOrAbort))

  inline def folderContents(inline path: String, ext: String): Map[String, String] = ${
    folderContents_impl('path, 'ext)
  }

  private def folderContents_impl(path: Expr[String], ext: Expr[String])(using Quotes): Expr[Map[String, String]] =
    Expr(readFolderContentsOf(path.valueOrAbort, ext.valueOrAbort)(identity))

  inline def jsonContent(inline path: String): Value = ${
    jsonContent_impl('path)
  }

  private def jsonContent_impl(path: Expr[String])(using Quotes): Expr[Value] =
    import JsonUtils.given
    Expr(parseJsonValue(readContentOf(path.valueOrAbort)))

  inline def folderJsonContents(path: String, ext: String): Map[String, Value] = ${
    folderJsonContents_impl('path, 'ext)
  }

  def folderJsonContents_impl(path: Expr[String], ext: Expr[String])(using Quotes): Expr[Map[String, Value]] =
    import JsonUtils.given
    Expr(readFolderContentsOf(path.valueOrAbort, ext.valueOrAbort)(parseJsonValue))
