package frawa.typedjson.macros

import frawa.typedjson.parser._

import scala.reflect.macros.blackbox
import scala.reflect.macros.blackbox.Context

object JsonUtils {
  private val parser = new ZioParser

  def toJsonValueExpr(c: Context)(value: Value): c.Expr[Value] = {
    import c.universe._
    implicit val l = JsonUtils.liftableJsonValue(c)
    c.Expr(q"""$value""")
  }

  def parseJsonValue(content: String): Value = {
    parser
      .parse(content)
      .swap
      .map { e =>
        throw new IllegalArgumentException(e)
      }
      .swap
      .toOption
      .get
  }

  def liftableJsonValue(c: blackbox.Context): c.universe.Liftable[Value] = {
    import c.universe._

    implicit def liftBigDecimal: Liftable[BigDecimal] = (v: BigDecimal) => q"""BigDecimal(${v.toString()})"""
    implicit def liftSeq: Liftable[Seq[Value]]        = (vs: Seq[Value]) => q"""Seq(..$vs)"""
    implicit def liftValue: Liftable[Value]           = (v: Value) => toExpr(v).tree

    def toExpr(value: Value) =
      value match {
        case NullValue       => c.Expr(q"NullValue")
        case StringValue(v)  => c.Expr(q"""StringValue($v)""")
        case BoolValue(v)    => c.Expr(q"""BoolValue($v)""")
        case NumberValue(v)  => c.Expr(q"""NumberValue(${v})""")
        case ArrayValue(vs)  => c.Expr(q"""ArrayValue(${vs})""")
        case ObjectValue(vs) => c.Expr(q"""ObjectValue(${vs})""")
      }

    Liftable((v: Value) => toExpr(v).tree)
  }

}
