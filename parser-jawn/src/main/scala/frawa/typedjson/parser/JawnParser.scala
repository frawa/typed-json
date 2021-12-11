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

import frawa.typedjson.pointer.{OffsetParser, Pointer}
import org.typelevel.jawn
import org.typelevel.jawn.FContext

class JawnParser extends Parser with OffsetParser {

  override def parse(json: String): Either[String, Value] = {
    jawn.Parser.parseFromString(json)(valueFacade).toEither.swap.map(_.getMessage).swap
  }

  override def pointerAt(json: String)(offset: Int): Either[String, Pointer] = {
    jawn.Parser
      .parseFromString(json)(pointerFacade(offset))
      .toEither
      .swap
      .map(_.getMessage)
      .swap
      .map(_.getOrElse(Pointer.empty))
  }

  private val valueFacade: jawn.Facade[Value] = new jawn.Facade.SimpleFacade[Value] {
    override def jarray(vs: List[Value]): Value                             = ArrayValue(vs)
    override def jobject(vs: Map[String, Value]): Value                     = ObjectValue(vs)
    override def jnull: Value                                               = NullValue
    override def jfalse: Value                                              = BoolValue(false)
    override def jtrue: Value                                               = BoolValue(true)
    override def jnum(s: CharSequence, decIndex: Int, expIndex: Int): Value = NumberValue(BigDecimal.exact(s.toString))
    override def jstring(s: CharSequence): Value                            = StringValue(s.toString)
  }

  private def pointerFacade(atOffset: Int): jawn.Facade[Option[Pointer]] = new jawn.Facade[Option[Pointer]] {

    override def singleContext(index: Int): FContext[Option[Pointer]] = new FContext[Option[Pointer]] {
      private var current: Option[Pointer]                   = None
      override def add(s: CharSequence, index: Int): Unit    = {}
      override def add(v: Option[Pointer], index: Int): Unit = { current = v }
      override def finish(index: Int): Option[Pointer] = current.orElse(
        Option(Pointer.empty)
          .filter(_ => index <= atOffset)
      )
      override def isObj: Boolean = false
    }

    override def arrayContext(index0: Int): FContext[Option[Pointer]] = new FContext[Option[Pointer]] {
      private var current: Option[(Int, Pointer)] = None

      override def add(s: CharSequence, index: Int): Unit = {
        if (index <= atOffset) {
          current = Option((current.map(_._1).map(_ + 1).getOrElse(0), Pointer.empty))
        }
      }
      override def add(v: Option[Pointer], index: Int): Unit = {
        lazy val i = current.map(_._1).map(_ + 1).getOrElse(0)
        current = v.map(p => if (index < atOffset) (i, Pointer.empty) else (i, p)).orElse(current)
      }
      override def finish(index: Int): Option[Pointer] = {
        current.map { case (i, v) => (Pointer.empty / i) / v }
      }

      override def isObj: Boolean = false
    }

    override def objectContext(index: Int): FContext[Option[Pointer]] = new FContext[Option[Pointer]] {
      private var current: Option[(String, Pointer)]         = None
      override def add(s: CharSequence, index: Int): Unit    = None
      override def add(v: Option[Pointer], index: Int): Unit = None
      override def finish(index: Int): Option[Pointer]       = None
      override def isObj: Boolean                            = true
    }

    override def jnull(index: Int): Option[Pointer] = Option(Pointer.empty).filter(_ => index <= atOffset)

    override def jfalse(index: Int): Option[Pointer] = Option(Pointer.empty).filter(_ => index <= atOffset)

    override def jtrue(index: Int): Option[Pointer] = Option(Pointer.empty).filter(_ => index <= atOffset)

    override def jnum(s: CharSequence, decIndex: Int, expIndex: Int, index: Int): Option[Pointer] =
      Option(Pointer.empty).filter(_ => index <= atOffset)

    override def jstring(s: CharSequence, index: Int): Option[Pointer] =
      Option(Pointer.empty).filter(_ => index <= atOffset)
  }

}
