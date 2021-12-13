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
import org.typelevel.jawn
import org.typelevel.jawn.FContext

//import scala.collection.mutable

class JawnParser extends Parser with OffsetParser {
  override def parse(json: String): Either[String, Value] = {
    jawn.Parser.parseFromString(json)(valueFacade).toEither.swap.map(_.toString).swap
  }

  override def parseWithOffset(json: String): Either[String, Offset.Value] = {
    jawn.Parser
      .parseFromString(json)(offsetValueFacade)
      .toEither
      .swap
      .map { ex =>
//        ex.printStackTrace()
        ex.toString
      }
      .swap
  }

  override def pointerAt(value: Offset.Value)(at: Int): Pointer = {
    def go(value: Offset.Value): Option[Pointer] = {
      value match {
        case Offset.ArrayValue(offset, vs) =>
          if (offset.contains(at)) {
            vs.zipWithIndex
              .find(_._1.offset.contains(at))
              .flatMap { case (v, i) =>
                go(v).map(Pointer.empty / i / _)
              }
              .orElse(Some(Pointer.empty))
          } else {
            None
          }
        case Offset.ObjectValue(offset, properties) =>
          if (offset.contains(at)) {
            properties
              .find(_._2.offset.contains(at))
              .flatMap { case (k, v) =>
                val prefix = Pointer.empty / k.value.toString
                go(v).map(prefix / _).orElse(Some(prefix))
              }
              .orElse(
                properties.keys
                  .find(_.offset.contains(at))
                  .map(_ => Pointer.empty)
              )
              .orElse(Some(Pointer.empty))
          } else {
            None
          }
        case _ => Some(Pointer.empty)
      }
    }
    go(value).getOrElse(Pointer.empty)
  }

  override def offsetAt(value: Offset.Value)(pointer: Pointer): Offset = ???

  private val valueFacade: jawn.Facade[Value] = new jawn.Facade.SimpleFacade[Value] {
    override def jarray(vs: List[Value]): Value         = Value.ArrayValue(vs)
    override def jobject(vs: Map[String, Value]): Value = Value.ObjectValue(vs)
    override def jnull: Value                           = Value.NullValue
    override def jfalse: Value                          = Value.BoolValue(false)
    override def jtrue: Value                           = Value.BoolValue(true)
    override def jnum(s: CharSequence, decIndex: Int, expIndex: Int): Value =
      Value.NumberValue(BigDecimal.exact(s.toString))
    override def jstring(s: CharSequence): Value = Value.StringValue(s.toString)
  }

  private def offsetValueFacade: jawn.Facade[Offset.Value] = new jawn.Facade[Offset.Value] {

    private def string(s: CharSequence, index: Int): Offset.StringValue =
      Offset.StringValue(Offset(index, index + s.length() + 2), s)

    override def singleContext(index: Int): FContext[Offset.Value] = new FContext[Offset.Value] {
      private var current: Option[Offset.Value] = None
      override def add(s: CharSequence, index: Int): Unit = {
        current = Some(string(s, index))
      }
      override def add(v: Offset.Value, index: Int): Unit = {
        current = Some(v)
      }
      override def finish(index: Int): Offset.Value =
        current.getOrElse(throw new IllegalStateException("missing single value"))
      override def isObj: Boolean = false
    }

    override def arrayContext(startIndex: Int): FContext[Offset.Value] = new FContext[Offset.Value] {
      private var vs: Seq[Offset.Value] = Seq.empty

      override def add(s: CharSequence, index: Int): Unit = {
        vs = vs :+ string(s, index)
      }
      override def add(v: Offset.Value, index: Int): Unit = {
        vs = vs :+ v
      }
      override def finish(index: Int): Offset.Value = {
        Offset.ArrayValue(Offset(startIndex, index + 1), vs)
      }

      override def isObj: Boolean = false
    }

    override def objectContext(startIndex: Int): FContext[Offset.Value] = new FContext[Offset.Value] {
      private var currentKey: Option[Offset.StringValue]            = None
      private var properties: Map[Offset.StringValue, Offset.Value] = Map.empty
      override def add(s: CharSequence, index: Int): Unit = {
        if (currentKey.isEmpty) {
          currentKey = Some(string(s, index))
        } else {
          properties = properties + (
            (
              currentKey.get,
              string(s, index)
            )
          )
          currentKey = None
        }
      }
      override def add(v: Offset.Value, index: Int): Unit = {
        properties = properties + ((currentKey.get, v))
        currentKey = None
      }
      override def finish(index: Int): Offset.Value = {
        Offset.ObjectValue(Offset(startIndex, index), properties)
      }
      override def isObj: Boolean = true
    }

    override def jnull(index: Int): Offset.Value = Offset.NullValue(Offset(index, index + 4))

    override def jfalse(index: Int): Offset.Value =
      Offset.BoolValue(Offset(index, index + 5), value = false)

    override def jtrue(index: Int): Offset.Value =
      Offset.BoolValue(Offset(index, index + 4), value = true)

    override def jnum(s: CharSequence, decIndex: Int, expIndex: Int, index: Int): Offset.Value =
      Offset.NumberValue(Offset(index, index + s.length()), BigDecimal.exact(s.toString))

    override def jstring(s: CharSequence, index: Int): Offset.Value =
      string(s, index)
  }

//  private def recoveringFacade[T](facade: jawn.Facade[T]): jawn.Facade[T] = new jawn.Facade[T] {
//
//    private val stack: mutable.ArrayDeque[FContext[T]] = mutable.ArrayDeque.empty[T]
//
//    private def delegatingContext(delegate: jawn.FContext[T]): jawn.FContext[T] = new FContext[T] {
//      stack.append(delegate)
//      override def add(s: CharSequence, index: Int): Unit = delegate.add(s, index)
//      override def add(v: T, index: Int): Unit            = delegate.add(v, index)
//      override def finish(index: Int): T                  = delegate.finish(index)
//      override def isObj: Boolean                         = delegate.isObj
//    }
//
//    override def singleContext(index: Int): FContext[T] = delegatingContext(facade.singleContext(index))
//    override def arrayContext(index: Int): FContext[T]  = delegatingContext(facade.singleContext(index))
//    override def objectContext(index: Int): FContext[T] = delegatingContext(facade.singleContext(index))
//
//    override def jnull(index: Int): T  = facade.jnull(index)
//    override def jfalse(index: Int): T = facade.jfalse(index)
//    override def jtrue(index: Int): T  = facade.jtrue(index)
//    override def jnum(s: CharSequence, decIndex: Int, expIndex: Int, index: Int): T =
//      facade.jnum(s, decIndex, expIndex, index)
//    override def jstring(s: CharSequence, index: Int): T = facade.jstring(s, index)
//  }
}
