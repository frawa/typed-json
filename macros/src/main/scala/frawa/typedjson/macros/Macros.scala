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

object Macros {
  import FileUtils._
  import JsonUtils._

  import scala.language.experimental.macros
  import scala.reflect.macros.blackbox._

  def fileContent(path: String): String = macro fileContent_impl

  def fileContent_impl(c: Context)(path: c.Expr[String]): c.Expr[String] = {
    toStringExpr(c)(readContentOf(fromExpr(c)(path)))
  }

  def jsonContent(path: String): Value = macro jsonContent_impl

  def jsonContent_impl(c: Context)(path: c.Expr[String]): c.Expr[Value] = {
    toJsonValueExpr(c)(parseJsonValue(readContentOf(fromExpr(c)(path))))
  }

  def folderContents(path: String, ext: String): Map[String, String] = macro folderContents_impl

  def folderContents_impl(
      c: Context
  )(path: c.Expr[String], ext: c.Expr[String]): c.Expr[Map[String, String]] = {
    import c.universe._
    val content = readFolderContentsOf(fromExpr(c)(path), fromExpr(c)(ext))(identity)
    c.Expr(q"""$content""")
  }

  def folderJsonContents(path: String, ext: String): Map[String, Value] = macro folderJsonContents_impl

  def folderJsonContents_impl(
      c: Context
  )(path: c.Expr[String], ext: c.Expr[String]): c.Expr[Map[String, Value]] = {
    import c.universe._
    val content    = readFolderContentsOf(fromExpr(c)(path), fromExpr(c)(ext))(parseJsonValue)
    implicit val l = JsonUtils.liftableJsonValue(c)
    c.Expr(q"""$content""")
  }

}
