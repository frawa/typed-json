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

import java.nio.file.{Files, Paths}
import scala.io.Source
// import scala.reflect.macros.blackbox.Context
import scala.util.Using

object FileUtils {

  def readContentOf(path: String): String = Using.resource(Source.fromFile(path))(_.getLines().mkString("\n"))

  def readFolderContentsOf[T](path: String, ext: String)(f: String => T): Map[String, T] = {
    import scala.jdk.CollectionConverters._
    Files
      .list(Paths.get(path))
      .iterator
      .asScala
      .toSeq
      .filterNot(_.toFile.isDirectory)
      .filter(_.getFileName.toString.endsWith(ext))
      .sortBy(_.getFileName.toString)
      .map(path => (path.getFileName.toString, f(readContentOf(path.toAbsolutePath.toString))))
      .toMap
  }

  /*
  def fromExpr(c: Context)(expr: c.Expr[String]): String = {
    import c.universe._
    // WONTWORK, pulls closure into scope
    // val value = c.processing(c.Expr[String](c.untypecheck(expr.tree)))
    val Literal(Constant(value: String)) = expr.tree
    value
  }

  def toStringExpr(c: Context)(content: String): c.Expr[String] = {
    import c.universe._
    c.Expr(Literal(Constant(content)))
  }

  def readContentOf(path: String): String = Using.resource(Source.fromFile(path))(_.getLines().mkString("\n"))

  def readFolderContentsOf[T](path: String, ext: String)(f: String => T): Map[String, T] = {
    import scala.jdk.CollectionConverters._
    Files
      .list(Paths.get(path))
      .iterator
      .asScala
      .toSeq
      .filterNot(_.toFile.isDirectory)
      .filter(_.getFileName.toString.endsWith(ext))
      .sortBy(_.getFileName.toString)
      .map(path => (path.getFileName.toString, f(readContentOf(path.toAbsolutePath.toString))))
      .toMap
  }
   */
}
