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

trait OffsetParser:
  import Offset.*
  import OffsetParser.ParseError
  def parseWithOffset(json: String): Either[ParseError, Value]

enum OffsetContext(val pointer: Pointer, val offset: Offset):
  case InsideKey(override val pointer: Pointer, override val offset: Offset)   extends OffsetContext(pointer, offset)
  case InsideValue(override val pointer: Pointer, override val offset: Offset) extends OffsetContext(pointer, offset)
  case NewKey(override val pointer: Pointer, override val offset: Offset)      extends OffsetContext(pointer, offset)
  case NewValue(override val pointer: Pointer, override val offset: Offset)    extends OffsetContext(pointer, offset)

  def mapPointer(f: Pointer => Pointer): OffsetContext =
    this match {
      case InsideKey(pointer, offset)   => InsideKey(f(pointer), offset)
      case InsideValue(pointer, offset) => InsideValue(f(pointer), offset)
      case NewKey(pointer, offset)      => NewKey(f(pointer), offset)
      case NewValue(pointer, offset)    => NewValue(f(pointer), offset)
    }
object OffsetParser:
  case class ParseError(offset: Int, message: String, recoveredValue: Option[Offset.Value])

  def contextAt(value: Offset.Value)(at: Int): OffsetContext =
    def go(value: Offset.Value): OffsetContext =
      value match
        case Offset.ArrayValue(_, vs) =>
          vs.zipWithIndex
            .find(_._1.offset.contains(at))
            .map { (v, i) =>
              go(v).mapPointer(Pointer.empty / i / _)
            }
            .orElse {
              vs.zipWithIndex.flatMap { (v, i) =>
                if at < v.offset.start then Some(OffsetContext.NewValue(Pointer.empty, Offset(at, at)))
                else if v.offset.end <= at then Some(OffsetContext.NewValue(Pointer.empty, Offset(at, at)))
                else None
              }.headOption
            }
            .getOrElse {
              OffsetContext.NewValue(Pointer.empty, Offset(at, at))
            }
        case ObjectValue(offset, properties) =>
          properties
            .find(_._2.offset.contains(at))
            .map { (k, v) =>
              go(v).mapPointer(Pointer.empty / k.value.toString() / _)
            }
            .orElse {
              properties
                .find(_._1.offset.contains(at))
                .map { (k, _) =>
                  val keyValue = go(k).mapPointer(Pointer.empty / k.value.toString() / _)
                  keyValue match {
                    case OffsetContext.InsideValue(pointer, offset) => OffsetContext.InsideKey(pointer, offset)
                    case _ => // TODO cannot happen
                      ???
                  }
                }
            }
            .orElse {
              properties.flatMap { (k, v) =>
                if offset.start < at && at < k.offset.start then
                  Some(OffsetContext.NewKey(Pointer.empty, Offset(at, at)))
                else if k.offset.end <= at && at < v.offset.start then
                  Some(OffsetContext.InsideValue(Pointer.empty / k.value.toString(), Offset(at, v.offset.start)))
                // else if v.offset.end <= at && at < offset.end then
                //   println(s"FW ${v.offset.end} at ${at}")
                //   Some(OffsetContext.NewKey(Pointer.empty, Offset(at, 1300 + at)))
                else None
              }.headOption
            }
            .getOrElse {
              if at == offset.start then OffsetContext.NewValue(Pointer.empty, Offset(at, at))
              else OffsetContext.NewKey(Pointer.empty, Offset(at, at))
            }
        case _ => OffsetContext.InsideValue(Pointer.empty, value.offset)
    if value.offset.contains(at) then go(value)
    else if value.offset.end <= at then OffsetContext.NewValue(Pointer.empty, Offset(at, at))
    else OffsetContext.NewValue(Pointer.empty, Offset(at, at))

  def pointerAt(value: Offset.Value)(at: Int): Pointer =
    def go(value: Offset.Value): Pointer =
      value match
        case Offset.ArrayValue(offset, vs) =>
          vs.zipWithIndex
            .find(_._1.offset.contains(at))
            .flatMap { (v, i) =>
              Some(Pointer.empty / i / go(v))
            }
            .orElse {
              if offset.end == at then Some(Pointer.empty)
              else
                vs.zipWithIndex
                  .filter(_._1.offset.end <= at)
                  .sortBy(_._1.offset.end)
                  .lastOption
                  .map((_, i) => (Pointer.empty / i))
            }
            .getOrElse {
              Pointer.empty
            }
        case Offset.ObjectValue(offset, properties) =>
          properties
            .find(_._2.offset.contains(at))
            .flatMap { case (k, v) =>
              val prefix = Pointer.empty / k.value.toString
              Some(prefix / go(v))
            }
            .orElse(
              properties.keys
                .find(_.offset.contains(at))
                .map(k => (Pointer.empty / k.value.toString).insideKey)
            )
            .orElse {
              if offset.end == at then Some(Pointer.empty)
              else
                properties.view
                  .flatMap { (k, v) =>
                    Seq(
                      // after value: edit next key
                      (v, Pointer.empty.insideKey),
                      // after key. edit value
                      (k, Pointer.empty / k.value.toString)
                    )
                  }
                  .toSeq
                  .filter(_._1.offset.end <= at)
                  .sortBy(_._1.offset.end)
                  .lastOption
                  .map(_._2)
            }
            .getOrElse {
              Pointer.empty
            }
        case _ => Pointer.empty
    Some(value)
      .filter(_.offset.contains(at))
      .map(go)
      .getOrElse(Pointer.empty)

  def offsetAt(value: Offset.Value)(pointer: Pointer): Option[Offset] =
    if pointer.isInsideKey && pointer.segments.nonEmpty then
      // TODO move into Pointer
      val FieldToken(key) = pointer.segments.last: @unchecked
      pointer.outer(value) match
        case Some(ObjectValue(_, properties)) => properties.find(_._1.value == key).map(_._1.offset)
        case _                                => None
    else pointer(value).map(_.offset)

case class Offset(start: Int, end: Int):
  def contains(at: Int): Boolean = start <= at && at < end

object Offset:
  import frawa.typedjson.parser.{Value as ValueWO}

  sealed trait Value:
    val offset: Offset

  case class NumberValue(offset: Offset, value: BigDecimal)                   extends Value
  case class BoolValue(offset: Offset, value: Boolean)                        extends Value
  case class NullValue(offset: Offset)                                        extends Value
  case class StringValue(offset: Offset, value: CharSequence)                 extends Value
  case class ArrayValue(offset: Offset, vs: Seq[Value])                       extends Value
  case class ObjectValue(offset: Offset, properties: Map[StringValue, Value]) extends Value

  def withoutOffset(value: Value): ValueWO =
    value match
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
