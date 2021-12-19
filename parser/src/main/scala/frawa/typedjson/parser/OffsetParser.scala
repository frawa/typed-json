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

import frawa.typedjson.pointer.Pointer

trait OffsetParser {
  import Offset._
  import OffsetParser.ParseError

  def parseWithOffset(json: String): Either[ParseError, Value]

  def pointerAt(value: Value)(offset: Int): Pointer
  def offsetAt(value: Value)(pointer: Pointer): Option[Offset]
}

object OffsetParser {
  case class ParseError(offset: Int, message: String)
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
