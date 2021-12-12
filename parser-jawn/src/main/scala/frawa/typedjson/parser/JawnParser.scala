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

class JawnParser extends Parser with OffsetParser {
  override def parse(json: String): Either[String, Value] = {
    jawn.Parser.parseFromString(json)(valueFacade).toEither.swap.map(_.getMessage).swap
  }

  override def parseWithOffset(json: String): Either[String, Offset.Value] = {
    jawn.Parser.parseFromString(json)(offsetValueFacade).toEither.swap.map(_.getMessage).swap
  }

  override def pointerAt(value: Offset.Value)(offset: Int): Pointer    = ???
  override def offsetAt(value: Offset.Value)(pointer: Pointer): Offset = ???

  private val valueFacade: jawn.Facade[Value] = new jawn.Facade.SimpleFacade[Value] {
    override def jarray(vs: List[Value]): Value                             = ArrayValue(vs)
    override def jobject(vs: Map[String, Value]): Value                     = ObjectValue(vs)
    override def jnull: Value                                               = NullValue
    override def jfalse: Value                                              = BoolValue(false)
    override def jtrue: Value                                               = BoolValue(true)
    override def jnum(s: CharSequence, decIndex: Int, expIndex: Int): Value = NumberValue(BigDecimal.exact(s.toString))
    override def jstring(s: CharSequence): Value                            = StringValue(s.toString)
  }

  private def offsetValueFacade: jawn.Facade[Offset.Value] = new jawn.Facade[Offset.Value] {

    private def string(s: CharSequence, index: Int): Offset.StringValue =
      Offset.StringValue(Offset(index, index + s.length() + 2), s)

    override def singleContext(index: Int): FContext[Offset.Value] = new FContext[Offset.Value] {
      private var current: Option[Offset.Value] = None
      override def add(s: CharSequence, index: Int): Unit = {
        current = Some(string(s, index))
      }
      override def add(v: Offset.Value, index: Int): Unit = { current = Some(v) }
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
        Offset.ArrayValue(Offset(startIndex, index), vs)
      }

      override def isObj: Boolean = false
    }

    override def objectContext(startIndex: Int): FContext[Offset.Value] = new FContext[Offset.Value] {
      private var currentKey: Option[String]            = None
      private var properties: Map[String, Offset.Value] = Map.empty
      override def add(s: CharSequence, index: Int): Unit = {
        if (currentKey.isEmpty) {
          currentKey = Some(s.toString)
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
        currentKey = null
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

}
