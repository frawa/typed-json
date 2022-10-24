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

package frawa.typedjson.parser.zio

import frawa.typedjson.parser.{Parser, Value}
import zio.json.*
import zio.json.ast.Json
import zio.json.ast.Json.{Arr, Obj}

class ZioParser extends Parser:
  import frawa.typedjson.parser.Value.*

  override def parse(json: String): Either[String, Value] =
    val result = json.fromJson[Json]
    result.map(toValue)

  private def toValue(ast: Json): Value =
    ast match
      case Json.Num(value)  => NumberValue(value)
      case Json.Bool(value) => BoolValue(value)
      case Json.Null        => NullValue
      case Json.Str(value)  => StringValue(value)
      case Arr(elements)    => ArrayValue(elements.map(toValue))
      case Obj(fields)      => ObjectValue(Map.from(fields.map(p => (p._1, toValue(p._2)))))
