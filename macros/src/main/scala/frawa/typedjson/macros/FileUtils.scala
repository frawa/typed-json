package frawa.typedjson.macros

import java.nio.file.{Files, Paths}
import scala.io.Source
import scala.reflect.macros.blackbox.Context
import scala.util.Using

object FileUtils {

  def fromExpr(c: Context)(expr: c.Expr[String]): String = {
    import c.universe._
    // WONTWORK, pulls closure into scope
    // val value = c.eval(c.Expr[String](c.untypecheck(expr.tree)))
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
}
