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

import org.typelevel.jawn

class JawnParser extends Parser {
  implicit val facade: jawn.Facade[Value] = new jawn.Facade.SimpleFacade[Value] {
    override def jarray(vs: List[Value]): Value = ArrayValue(vs)

    override def jobject(vs: Map[String, Value]): Value = ObjectValue(vs)

    override def jnull: Value = NullValue

    override def jfalse: Value = BoolValue(false)

    override def jtrue: Value = BoolValue(true)

    override def jnum(s: CharSequence, decIndex: Int, expIndex: Int): Value = NumberValue(BigDecimal.exact(s.toString))

    override def jstring(s: CharSequence): Value = StringValue(s.toString)
  }

  override def parse(json: String): Either[String, Value] = {
    jawn.Parser.parseFromString(json).toEither.swap.map(_.getMessage).swap
  }

}
