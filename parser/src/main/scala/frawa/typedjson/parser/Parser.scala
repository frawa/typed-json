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

sealed trait Value
case class NumberValue(value: BigDecimal)              extends Value
case class BoolValue(value: Boolean)                   extends Value
case object NullValue                                  extends Value
case class StringValue(value: String)                  extends Value
case class ArrayValue(items: Seq[Value])               extends Value
case class ObjectValue(properties: Map[String, Value]) extends Value

trait Parser {
  def parse(json: String): Either[String, Value]
}

object Parser {
  def apply(json: String)(implicit parser: Parser): Either[String, Value] = {
    parser.parse(json)
  }
}

object Value {
  def asString(value: Value): Option[String] = value match {
    case StringValue(v) => Some(v)
    case _              => None
  }

  def asBool(value: Value): Option[Boolean] = value match {
    case BoolValue(v) => Some(v)
    case _            => None
  }

  def asStrings(values: Seq[Value]): Seq[String] = values.flatMap(asString)

  def withoutOffset(value: Offset.Value): Value = {
    value match {
      case Offset.NumberValue(_, value)      => NumberValue(value)
      case Offset.BoolValue(_, value)        => BoolValue(value)
      case Offset.NullValue(_)               => NullValue
      case Offset.StringValue(_, value)      => StringValue(value.toString)
      case Offset.ArrayValue(_, vs)          => ArrayValue(vs.map(withoutOffset))
      case Offset.ObjectValue(_, properties) => ObjectValue(properties.view.mapValues(withoutOffset).toMap)
    }
  }
}
