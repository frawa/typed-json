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

package frawa.typedjson.macros

import frawa.typedjson.parser.Value
import frawa.typedjson.parser.jawn.JawnParser

// import scala.reflect.macros.blackbox
// import scala.reflect.macros.blackbox.Context

object JsonUtils {
  import scala.quoted.*
  import Value._

  private val parser = new JawnParser

  def parseJsonValue(content: String): Value = {
    parser
      .parse(content)
      .swap
      .map { e =>
        throw new IllegalArgumentException(e)
      }
      .swap
      .toOption
      .get
  }

  given [T <: Value]: ToExpr[T] with {
    def apply(value: T)(using Quotes) = {
      Expr(value)
    }
  }

  given ToExpr[Value] with {
    def apply(value: Value)(using Quotes) = {
      value match {
        case NullValue       => Expr(NullValue)
        case StringValue(v)  => Expr(StringValue(v))
        case BoolValue(v)    => Expr(BoolValue(v))
        case NumberValue(v)  => Expr(NumberValue(v))
        case ArrayValue(vs)  => Expr(ArrayValue(vs))
        case ObjectValue(vs) => Expr(ObjectValue(vs))
      }
    }
  }

}
