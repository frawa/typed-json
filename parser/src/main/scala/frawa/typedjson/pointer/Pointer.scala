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

package frawa.typedjson.pointer

import frawa.typedjson.parser.Offset
import frawa.typedjson.parser.Value
import frawa.typedjson.parser.Value.ArrayValue
import frawa.typedjson.parser.Value.ObjectValue

import scala.collection.immutable.Seq

object Pointer:
  def empty = new Pointer(Nil)

  def apply(index: Int): Pointer    = Pointer.empty / index
  def apply(field: String): Pointer = Pointer.empty / field

  def parse(spec: String): Option[Pointer] =
    if spec == null then Some(Pointer.empty)
    else
      sequence(
        spec
          .split("/", -1)
          .toIndexedSeq
          .map(unescape)
          .drop(1)
      )
        .map { fields =>
          fields.map(t => t.toIntOption.map(ArrayIndexToken.apply).getOrElse(FieldToken(t)))
        }
        .map(tokens => Pointer(tokens))

  private def sequence[A](cs: Seq[Option[A]]): Option[Seq[A]] =
    cs.foldLeft(Option(Seq.empty[A])) { (acc, c) =>
      acc.flatMap(cs => c.map(cs :+ _))
    }

  private def unescape(field: String): Option[String] =
    field
      .foldLeft(Option(("", false))) {
        case (Some((s, false)), '~') => Some((s, true))
        case (Some((s, true)), '0')  => Some((s :+ '~', false))
        case (Some((s, true)), '1')  => Some((s :+ '/', false))
        case (Some((s, false)), c)   => Some((s :+ c, false))
        case _                       => None
      }
      .filterNot(_._2)
      .map(_._1)

case class Pointer(segments: Seq[Token]):
  override def toString: String =
    if this.segments.isEmpty then ""
    else "/" + this.segments.mkString("/")

  def /(index: Int): Pointer =
    Pointer(segments :+ ArrayIndexToken(index))
  def /(field: String): Pointer =
    Pointer(segments :+ FieldToken(field))
  def /(pointer: Pointer): Pointer =
    Pointer(segments ++ pointer.segments)

  def outer: Pointer =
    Pointer(segments.dropRight(1))

  def apply(value: Value): Option[Value] = segments.foldLeft(Option(value)) { case (v, segment) =>
    v.flatMap(v =>
      segment match {
        case ArrayIndexToken(index) =>
          v match {
            case ArrayValue(vs)          => vs.lift(index)
            case ObjectValue(properties) => properties.get(index.toString)
            case _                       => None
          }
        case FieldToken(field) =>
          v match {
            case ObjectValue(properties) => properties.get(field)
            case _                       => None
          }
        case _ => None
      }
    )
  }

  // TODO dedup wrt apply(value: Value)
  def apply(value: Offset.Value): Option[Offset.Value] = segments.foldLeft(Option(value)) {
    case (v, segment) =>
      v.flatMap(v =>
        segment match {
          case ArrayIndexToken(index) =>
            v match {
              case Offset.ArrayValue(_, vs) => vs.lift(index)
              case Offset.ObjectValue(_, properties) =>
                properties.find(_._1.value == index.toString).map(_._2)
              case _ => None
            }
          case FieldToken(field) =>
            v match {
              case Offset.ObjectValue(_, properties) =>
                properties.find(_._1.value == field).map(_._2)
              case _ => None
            }
          case _ => None
        }
      )
  }

  def targetField: Option[String] =
    segments.lastOption match {
      case Some(FieldToken(field)) => Some(field)
      case _                       => None
    }

trait Token

case class ArrayIndexToken(index: Int) extends Token:
  override def toString: String = { index.toString }

case class FieldToken(field: String) extends Token:
  override def toString: String =
    field
      .replace("~", "~0")
      .replace("/", "~1")
