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

import frawa.typedjson.parser.Parser
import munit._

class ZioParserTest extends FunSuite:
  import frawa.typedjson.parser.Value.*

  given Parser = new ZioParser()

  test("basic types") {
    assertEquals(Parser("""13"""), Right(NumberValue(13)))
    assertEquals(Parser("""true"""), Right(BoolValue(true)))
    assertEquals(Parser("""null"""), Right(NullValue))
    assertEquals(Parser(""""string""""), Right(StringValue("string")))
  }

  test("array") {
    assertEquals(Parser("""[13]"""), Right(ArrayValue(Seq(NumberValue(13)))))
  }

  test("object") {
    assertEquals(
      Parser("""{"toto":"titi"}"""),
      Right(ObjectValue(Map("toto" -> StringValue("titi"))))
    )
  }

  test("big number") {
    assertEquals(
      Parser("98249283749234923498293171823948729348710298301928331"),
      Right(NumberValue(BigDecimal("98249283749234923498293171823948729348710298301928331")))
    )
  }
