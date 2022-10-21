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

package frawa.typedjson.parser

import frawa.typedjson.parser.Offset.ObjectValue
import frawa.typedjson.pointer.{FieldToken, Pointer}

trait OffsetParser {
  import Offset._
  import OffsetParser.ParseError
  def parseWithOffset(json: String): Either[ParseError, Value]
}

object OffsetParser {
  case class ParseError(offset: Int, message: String, recoveredValue: Option[Offset.Value])

  def pointerAt(value: Offset.Value)(at: Int): Pointer = {
    def go(value: Offset.Value): Option[Pointer] = {
      value match {
        case Offset.ArrayValue(_, vs) =>
          vs.zipWithIndex
            .find(_._1.offset.contains(at))
            .flatMap { case (v, i) =>
              go(v).map(Pointer.empty / i / _)
            }
            .orElse(Some(Pointer.empty))
        case Offset.ObjectValue(_, properties) =>
          properties
            .find(_._2.offset.contains(at))
            .flatMap { case (k, v) =>
              val prefix = Pointer.empty / k.value.toString
              go(v).map(prefix / _).orElse(Some(prefix))
            }
            .orElse(
              properties.keys
                .find(_.offset.contains(at))
                .map(k => (Pointer.empty / k.value.toString).insideKey)
            )
            .orElse(Some(Pointer.empty))
        case _ => Some(Pointer.empty)
      }
    }
    Some(value)
      .filter(_.offset.contains(at))
      .flatMap(go)
      .getOrElse(Pointer.empty)
  }

  def offsetAt(value: Offset.Value)(pointer: Pointer): Option[Offset] = {
    if pointer.isInsideKey then {
      // TODO move into Pointer
      val FieldToken(key) = pointer.segments.last: @unchecked
      pointer.outer(value) match {
        case Some(ObjectValue(_, properties)) => properties.find(_._1.value == key).map(_._1.offset)
        case _                                => None
      }
    } else {
      pointer(value).map(_.offset)
    }
  }
}

case class Offset(start: Int, end: Int) {
  // TODO end is exclusive, why is it inclusive here?
  def contains(at: Int): Boolean = start <= at && at <= end
}

object Offset {
  import frawa.typedjson.parser.{Value => ValueWO}

  sealed trait Value {
    val offset: Offset
  }

  case class NumberValue(offset: Offset, value: BigDecimal)                   extends Value
  case class BoolValue(offset: Offset, value: Boolean)                        extends Value
  case class NullValue(offset: Offset)                                        extends Value
  case class StringValue(offset: Offset, value: CharSequence)                 extends Value
  case class ArrayValue(offset: Offset, vs: Seq[Value])                       extends Value
  case class ObjectValue(offset: Offset, properties: Map[StringValue, Value]) extends Value

  def withoutOffset(value: Value): ValueWO = {
    value match {
      case Offset.NumberValue(_, value) => ValueWO.NumberValue(value)
      case Offset.BoolValue(_, value)   => ValueWO.BoolValue(value)
      case Offset.NullValue(_)          => ValueWO.NullValue
      case Offset.StringValue(_, value) => ValueWO.StringValue(value.toString)
      case Offset.ArrayValue(_, vs)     => ValueWO.ArrayValue(vs.map(withoutOffset))
      case Offset.ObjectValue(_, properties) =>
        ValueWO.ObjectValue(
          properties
            .map { case (key, value) =>
              Value.asString(withoutOffset(key)).map((_, withoutOffset(value)))
            }
            .flatten
            .toMap
        )
    }
  }
}
