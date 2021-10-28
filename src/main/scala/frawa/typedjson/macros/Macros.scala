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
