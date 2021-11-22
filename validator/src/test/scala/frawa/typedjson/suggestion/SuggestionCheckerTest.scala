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

package frawa.typedjson.suggestion

import frawa.typedjson.parser._
import frawa.typedjson.testutil.TestSchemas.{numberArraySchema, totoObjectSchema, totoRequiredObjectSchema}
import frawa.typedjson.testutil.TestUtil.{assertResult, withSchema}
import frawa.typedjson.processor.{LoadedSchemasResolver, Pointer, SchemaValue}
import munit.FunSuite

class SuggestEvalTest extends FunSuite {
  implicit val zioParser: ZioParser = new ZioParser()

  private def assertSuggest(text: String, at: Pointer = Pointer.empty)(schema: SchemaValue)(
      f: Seq[Value] => Unit
  ) = {
    implicit val l: Option[LoadedSchemasResolver.LazyResolver] = None
    assertResult(SuggestionEval(at))(schema, text) { result =>
      f(result.results.flatMap(_.suggestions).distinct)
    }
  }

  test("suggest property") {
    withSchema(totoObjectSchema) { schema =>
      assertSuggest(
        """{
          |"toto": 13
          |}
          |""".stripMargin
      )(
        schema
      ) { result =>
        assertEquals(
          result,
          Seq(
            ObjectValue(Map()),
            ObjectValue(Map("toto" -> NumberValue(0))),
            ObjectValue(Map("titi" -> StringValue("")))
          )
        )
      }
    }
  }

  test("suggest deep") {
    withSchema("""{
                 |"$id": "testme",
                 |"type": "object", 
                 |"properties": { 
                 |  "foo": { 
                 |    "type": "object", 
                 |    "properties": { 
                 |      "bar": { "type": "number" }
                 |    }
                 |  }
                 |} 
                 |}
                 |""".stripMargin) { schema =>
      assertSuggest(
        """{
          |"foo": {}
          |}
          |""".stripMargin
      )(
        schema
      ) { result =>
        assertEquals(
          result,
          Seq(
            ObjectValue(Map()),
            ObjectValue(
              properties = Map(
                "foo" -> ObjectValue(
                  properties = Map()
                )
              )
            ),
            ObjectValue(Map("foo" -> ObjectValue(Map("bar" -> NumberValue(0)))))
          )
        )
      }
    }
  }

  test("suggest several values") {
    withSchema("""{
                 |"$id": "testme",
                 |"type": "object", 
                 |"properties": { 
                 |  "foo": { 
                 |    "type": "object", 
                 |    "properties": { 
                 |      "bar": { "type": "number" }
                 |    }
                 |  },
                 |  "gnu": { 
                 |    "type": "object", 
                 |    "properties": { 
                 |      "toto": { "type": "string" }
                 |    }
                 |  }
                 |} 
                 |}
                 |""".stripMargin) { schema =>
      assertSuggest(
        """{
          |"foo": {}
          |}
          |""".stripMargin
      )(
        schema
      ) { result =>
        assertEquals(
          result,
          Seq(
            ObjectValue(Map()),
            ObjectValue(
              properties = Map(
                "foo" -> ObjectValue(
                  properties = Map()
                )
              )
            ),
            ObjectValue(
              Map(
                "foo" -> ObjectValue(
                  Map(
                    "bar" ->
                      NumberValue(0)
                  )
                )
              )
            ),
            ObjectValue(
              Map(
                "gnu" ->
                  ObjectValue(Map())
              )
            ),
            ObjectValue(
              properties = Map(
                "gnu" -> ObjectValue(
                  properties = Map(
                    "toto" -> StringValue(
                      value = ""
                    )
                  )
                )
              )
            )
          )
        )
      }
    }
  }

  test("suggestions for several properties") {
    withSchema("""{
                 |"$id": "testme",
                 |"type": "object", 
                 |"properties": { 
                 |  "foo": { 
                 |    "type": "object", 
                 |    "properties": { 
                 |      "bar": { "type": "number" },
                 |      "gnu": { "type": "number" }
                 |    }
                 |  }
                 |} 
                 |}
                 |""".stripMargin) { schema =>
      assertSuggest(
        """{
          |"foo": {}
          |}
          |""".stripMargin
      )(
        schema
      ) { result =>
        assertEquals(
          result,
          Seq(
            ObjectValue(
              properties = Map()
            ),
            ObjectValue(
              properties = Map(
                "foo" -> ObjectValue(
                  properties = Map()
                )
              )
            ),
            ObjectValue(
              Map(
                "foo" -> ObjectValue(
                  Map(
                    "bar" ->
                      NumberValue(0)
                  )
                )
              )
            ),
            ObjectValue(
              Map(
                "foo" -> ObjectValue(
                  Map(
                    "gnu" ->
                      NumberValue(0)
                  )
                )
              )
            )
          )
        )
      }
    }
  }

  test("null or number") {
    withSchema("""{
                 |"$id": "testme",
                 |"type": ["null","number"]
                 |}""".stripMargin) { schema =>
      assertSuggest("""13""")(schema) { result =>
        assertEquals(
          result,
          Seq(NullValue, NumberValue(0))
        )
      }
      assertSuggest("""true""")(schema) { result =>
        assertEquals(
          result,
          Seq(NullValue, NumberValue(0))
        )
      }
    }
  }

  test("enum") {
    withSchema("""{
                 |"$id": "testme",
                 |"type": "number",
                 |"enum": [13, 14]                
                 |}""".stripMargin) { schema =>
      assertSuggest("""true""")(schema) { result =>
        assertEquals(
          result,
          Seq(NumberValue(0), NumberValue(13), NumberValue(14))
        )
      }
      assertSuggest("""13""")(schema) { result =>
        assertEquals(
          result,
          Seq(NumberValue(0), NumberValue(13), NumberValue(14))
        )
      }
    }
  }

  test("const") {
    withSchema("""{
                 |"$id": "testme",
                 |"type": "boolean",
                 |"const": true               
                 |}""".stripMargin) { schema =>
      assertSuggest("""true""")(schema) { result =>
        assertEquals(
          result,
          Seq(BoolValue(true))
        )
      }
      assertSuggest("""13""")(schema) { result =>
        assertEquals(
          result,
          Seq(BoolValue(true))
        )
      }
    }
  }

  test("discriminator") {
    withSchema("""{
                 |"$id": "testme",
                 |"oneOf": [{
                 |  "if": { 
                 |    "type": "object",
                 |    "properties": { 
                 |      "kind": { "type": "string", "const": "first" }
                 |    }
                 |  },
                 |  "then": {
                 |    "type": "object",
                 |    "properties": { 
                 |      "gnu": { "type": "number" }
                 |    }
                 |  }
                 |},{
                 |  "if": { 
                 |    "type": "object",
                 |    "properties": { 
                 |      "kind": { "type": "string", "const": "second" }
                 |    }
                 |  },
                 |  "then": {
                 |    "type": "object",
                 |    "properties": { 
                 |      "foo": { "type": "boolean" }
                 |    }
                 |  }
                 |}]
                 |}""".stripMargin) { schema =>
      assertSuggest("""{}""".stripMargin)(schema) { result =>
        assertEquals(
          result,
          Seq(
            ObjectValue(
              properties = Map()
            ),
            ObjectValue(
              properties = Map(
                "kind" -> StringValue(
                  value = ""
                )
              )
            ),
            ObjectValue(
              properties = Map(
                "kind" -> StringValue(
                  value = "first"
                )
              )
            ),
            ObjectValue(
              properties = Map(
                "gnu" -> NumberValue(
                  value = 0
                )
              )
            ),
            ObjectValue(
              properties = Map(
                "kind" -> StringValue(
                  value = "second"
                )
              )
            ),
            ObjectValue(
              properties = Map(
                "foo" -> BoolValue(
                  value = true
                )
              )
            )
          )
        )
      }
      assertSuggest("""{"kind":"first"}""")(schema) { result =>
        assertEquals(
          result,
          Seq(
            ObjectValue(
              properties = Map()
            ),
            ObjectValue(
              properties = Map(
                "kind" -> StringValue(
                  value = ""
                )
              )
            ),
            ObjectValue(
              properties = Map(
                "kind" -> StringValue(
                  value = "first"
                )
              )
            ),
            ObjectValue(
              properties = Map(
                "gnu" -> NumberValue(
                  value = 0
                )
              )
            ),
            ObjectValue(
              properties = Map(
                "kind" -> StringValue(
                  value = "second"
                )
              )
            ),
            ObjectValue(
              properties = Map(
                "foo" -> BoolValue(
                  value = true
                )
              )
            )
          )
        )
      }
    }
  }

  test("suggestions enum properties") {
    withSchema("""{
                 |"$id": "testme",
                 |"type": "object", 
                 |"properties": { 
                 |  "foo": { 
                 |    "type": "object", 
                 |    "properties": { 
                 |      "bar": { "type": "number", "enum": [13, 14] },
                 |      "gnu": { "type": "string", "enum": ["toto", "titi"] }
                 |    }
                 |  }
                 |} 
                 |}
                 |""".stripMargin) { schema =>
      assertSuggest(
        """{
          |"foo": {}
          |}
          |""".stripMargin
      )(
        schema
      ) { result =>
        assertEquals(
          result,
          Seq(
            ObjectValue(
              properties = Map()
            ),
            ObjectValue(Map("foo" -> ObjectValue(Map()))),
            ObjectValue(Map("foo" -> ObjectValue(Map("bar" -> NumberValue(0))))),
            ObjectValue(Map("foo" -> ObjectValue(Map("bar" -> NumberValue(13))))),
            ObjectValue(Map("foo" -> ObjectValue(Map("bar" -> NumberValue(14))))),
            ObjectValue(Map("foo" -> ObjectValue(Map("gnu" -> StringValue(""))))),
            ObjectValue(Map("foo" -> ObjectValue(Map("gnu" -> StringValue("toto"))))),
            ObjectValue(Map("foo" -> ObjectValue(Map("gnu" -> StringValue("titi")))))
          )
        )
      }
    }
  }

  test("suggest required property") {
    withSchema(totoRequiredObjectSchema) { schema =>
      assertSuggest(
        """{
          |"toto": 13
          |}
          |""".stripMargin
      )(
        schema
      ) { result =>
        assertEquals(
          result,
          Seq(
            ObjectValue(Map()),
            ObjectValue(
              properties = Map(
                "toto" -> NullValue,
                "gnu"  -> NullValue
              )
            ),
            ObjectValue(Map("toto" -> NumberValue(0))),
            ObjectValue(Map("gnu" -> BoolValue(true))),
            ObjectValue(Map("titi" -> StringValue("")))
          )
        )
      }
    }
  }

  test("suggest array") {
    withSchema(numberArraySchema) { schema =>
      assertSuggest(
        """[]""".stripMargin
      )(
        schema
      ) { result =>
        assertEquals(
          result,
          Seq(
            ArrayValue(Seq())
          )
        )
      }
    }
  }

  test("suggest at inside object") {
    withSchema(totoObjectSchema) { schema =>
      assertSuggest(
        """{
          |"toto": 13
          |}
          |""".stripMargin,
        Pointer.empty / "toto"
      )(
        schema
      ) { result =>
        assertEquals(
          result,
          Seq(
            NumberValue(0)
          )
        )
      }
    }
  }

  test("suggest item inside array") {
    withSchema(numberArraySchema) { schema =>
      assertSuggest(
        """[ 13, 14 ]""".stripMargin,
        Pointer.empty / 1
      )(
        schema
      ) { result =>
        assertEquals(
          result,
          Seq(
            NumberValue(0)
          )
        )
      }
    }
  }
}
