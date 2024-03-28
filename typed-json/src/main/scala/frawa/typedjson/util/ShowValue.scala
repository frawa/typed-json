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

package frawa.typedjson.util

import frawa.typedjson.parser.Value
import frawa.typedjson.parser.Value._

import scala.collection.immutable.Seq

object ShowValue:

  def prettyPrint(indent: Int = 0)(v: Value): String =
    def ws(indent: Int): String =
      String(Array.fill(2 * indent)(' '))

    import Ordering.Implicits.seqOrdering
    given Ordering[Value] with
      def compare(x: Value, y: Value): Int = (x, y) match {
        case (NullValue, NullValue)             => 0
        case (StringValue(xx), StringValue(yy)) => xx.compareTo(yy)
        case (NumberValue(xx), NumberValue(yy)) => xx.compareTo(yy)
        case (BoolValue(xx), BoolValue(yy))     => xx.compareTo(yy)
        case (ArrayValue(xx), ArrayValue(yy))   => seqOrdering.compare(xx.sorted, yy.sorted)
        case (ObjectValue(xx), ObjectValue(yy)) =>
          seqOrdering.compare(xx.toSeq.sorted, yy.toSeq.sorted)
        case _ => -1
      }

    given Ordering[Seq[Value]] with
      def compare(x: Seq[Value], y: Seq[Value]): Int =
        seqOrdering.compare(x, y)

    val sep  = s"\n${ws(indent)}"
    val sep1 = s"\n${ws(indent + 1)}"
    v match {
      case NullValue      => "null"
      case StringValue(v) => s"\"${v}\""
      case NumberValue(v) => s"${v}"
      case BoolValue(v)   => s"${v}"
      case ArrayValue(items) =>
        if items.isEmpty then "[]"
        else s"[${items.sorted.map(prettyPrint(indent + 1)).mkString(s",${sep1}")}${sep}]"
      case ObjectValue(properties) =>
        if properties.isEmpty then "{}"
        else
          s"{${sep1}${properties.toSeq.sorted
              .map { (p, v) =>
                s"\"${p}\": ${prettyPrint(indent + 1)(v)}"
              }
              .mkString(s",${sep1}")}${sep}}"
    }
