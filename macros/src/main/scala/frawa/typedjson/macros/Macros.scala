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

import scala.quoted.*

import frawa.inlinefiles.compiletime.FileContents

import frawa.typedjson.parser.Value
import JsonUtils.{given, *}
import scala.annotation.experimental
import java.nio.file.Path
import java.nio.file.Paths

@experimental
object Macros:
  import frawa.inlinefiles.InlineFilesWithHome.resolveHome

  inline def inlineJsonContents(path: String, ext: String)(homeSetting: String): Map[String, Value] = ${
    inlineJsonContents_impl('path, 'ext, 'homeSetting)
  }

  def inlineJsonContents_impl(path: Expr[String], ext: Expr[String], homeSetting: Expr[String])(using
      Quotes
  ): Expr[Map[String, Value]] =
    Expr(
      FileContents.parseTextContentsIn(
        path.valueOrAbort,
        ext.valueOrAbort,
        true,
        Some(resolveHome(homeSetting.valueOrAbort))
      )(parseJsonValue)
    )
