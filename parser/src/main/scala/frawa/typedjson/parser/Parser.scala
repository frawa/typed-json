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

trait Parser:
  def parse(json: String): Either[String, Value]

object Parser:
  def apply(json: String)(using parser: Parser): Either[String, Value] =
    parser.parse(json)

sealed trait Value
object Value:
  case class NumberValue(value: BigDecimal)              extends Value
  case class BoolValue(value: Boolean)                   extends Value
  case object NullValue                                  extends Value
  case class StringValue(value: String)                  extends Value
  case class ArrayValue(items: Seq[Value])               extends Value

  case class ObjectValue(properties: Map[String, Value]) extends Value

  def asString(value: Value): Option[String] = value match
    case StringValue(v) => Some(v)
    case _ => None

  def asBool(value: Value): Option[Boolean] = value match
    case BoolValue(v) => Some(v)
    case _ => None

  def asNumber(value: Value): Option[BigDecimal] = value match
    case NumberValue(v) => Some(v)
    case _ => None

  def asObject(value: Value): Option[Map[String, Value]] = value match
    case ObjectValue(v) => Some(v)
    case _ => None

  def asStrings(values: Seq[Value]): Seq[String] = values.flatMap(asString)

  def asArray(value: Value): Option[Seq[Value]] = value match
    case ArrayValue(v) => Some(v)
    case _ => None
