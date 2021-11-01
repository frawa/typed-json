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

import frawa.typedjson.parser.ArrayValue
import frawa.typedjson.parser.BoolValue
import frawa.typedjson.parser.NullValue
import frawa.typedjson.parser.NumberValue
import frawa.typedjson.parser.ObjectValue
import frawa.typedjson.parser.StringValue
import frawa.typedjson.parser.Value
import frawa.typedjson.parser.ZioParser

import scala.io.Source

object Macros {
  import scala.language.experimental.macros
  import scala.reflect.macros.blackbox.Context

  private val parser = new ZioParser

  def fileContent(path: String): String = macro fileContent_impl

  def fileContent_impl(c: Context)(path: c.Expr[String]): c.Expr[String] = {
    import c.universe._
    val Literal(Constant(pathValue: String)) = path.tree
    val content                              = contentOf(pathValue)
    c.Expr(Literal(Constant(content)))
  }

  def jsonContent(path: String): Value = macro jsonContent_impl

  def jsonContent_impl(c: Context)(path: c.Expr[String]): c.Expr[Value] = {
    import c.universe._
    val Literal(Constant(pathValue: String)) = path.tree
    val content                              = contentOf(pathValue)

    val value = parser
      .parse(content)
      .swap
      .map { e =>
        throw new IllegalArgumentException(e)
      }
      .swap
      .toOption
      .get

    implicit def liftValue: Liftable[Value]           = (v: Value) => toExpr(v).tree
    implicit def liftBigDecimal: Liftable[BigDecimal] = (v: BigDecimal) => q"""BigDecimal(${v.toString()})"""
    implicit def liftSeq: Liftable[Seq[Value]]        = (vs: Seq[Value]) => q"""Seq(..$vs)"""

    def toExpr(value: Value): c.Expr[Value] = {
      value match {
        case NullValue       => c.Expr(q"NullValue")
        case StringValue(v)  => c.Expr(q"""StringValue($v)""")
        case BoolValue(v)    => c.Expr(q"""BoolValue($v)""")
        case NumberValue(v)  => c.Expr(q"""NumberValue(${v})""")
        case ArrayValue(vs)  => c.Expr(q"""ArrayValue(${vs})""")
        case ObjectValue(vs) => c.Expr(q"""ObjectValue(${vs})""")
      }
    }
    c.Expr(q"""$value""")
  }

  private def contentOf(path: String): String = {
    Source.fromFile(path).getLines().mkString("\n")
  }
}
