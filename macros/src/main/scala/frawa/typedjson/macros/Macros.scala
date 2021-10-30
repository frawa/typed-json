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

import scala.io.Source

object Macros {
  import scala.language.experimental.macros
  import scala.reflect.macros.blackbox.Context

  def fileContent(path: String): String = macro fileContent_impl

  def fileContent_impl(c: Context)(path: c.Expr[String]): c.Expr[String] = {
    import c.universe._
    val Literal(Constant(pathValue: String)) = path.tree
    val content                              = Source.fromFile(pathValue).getLines.mkString("\n")
    c.Expr(Literal(Constant(content)))
  }
}
