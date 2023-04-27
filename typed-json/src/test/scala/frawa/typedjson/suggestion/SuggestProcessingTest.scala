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

import frawa.typedjson.keywords.{SchemaValue, Vocabulary}
import frawa.typedjson.meta.MetaSchemas
import frawa.typedjson.parser.*
import frawa.typedjson.parser.Value.*
import frawa.typedjson.pointer.Pointer
import frawa.typedjson.testutil.EvaluatorFactory
import frawa.typedjson.testutil.TestSchemas.{numberArraySchema, totoObjectSchema, totoRequiredObjectSchema}
import frawa.typedjson.testutil.TestUtil.{*, given}
import munit.FunSuite

@munit.IgnoreSuite
class SuggestProcessingTest extends FunSuite:

  private val vocabularyForTest = dialect(Seq(Vocabulary.coreId, Vocabulary.validationId, Vocabulary.applicatorId))

  private def factory(at: Pointer): EvaluatorFactory[SchemaValue, SuggestionOutput] =
    EvaluatorFactory.make(SuggestionProcessing(at), vocabularyForTest).mapResult(assertNoIgnoredKeywords)

  private def assertSuggest(text: String, at: Pointer = Pointer.empty)(schema: SchemaValue)(
      f: Set[Value] => Unit
  ) =
    given EvaluatorFactory[SchemaValue, SuggestionOutput] = factory(at)
    assertResult[SuggestionOutput](text)(schema) { result =>
      f(result.output.map(_.suggestions).getOrElse(Set()))
    }

  test("suggest one object per property") {
    withSchema(totoObjectSchema) { schema =>
      assertSuggest("""{"toto": 13}""")(
        schema
      ) { result =>
        assertEquals(
          result,
          Set(
            ObjectValue(Map()),
            ObjectValue(Map("toto" -> NumberValue(0))),
            ObjectValue(Map("titi" -> StringValue("")))
          )
        )
      }
    }
  }

  test("suggest property names inside key") {
    withSchema(totoObjectSchema) { schema =>
      assertSuggest("""{"toto": 13}""", (Pointer.empty / "toto").insideKey)(
        schema
      ) { result =>
        assertEquals(
          result,
          Set(
            StringValue("toto"),
            StringValue("titi")
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
      assertSuggest("""{"foo": {}}""")(
        schema
      ) { result =>
        assertEquals(
          result,
          Set(
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
      assertSuggest("""{"foo": {}}""")(
        schema
      ) { result =>
        assertEquals(
          result,
          Set(
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
      assertSuggest("""{"foo": {}}""")(
        schema
      ) { result =>
        assertEquals(
          result,
          Set(
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
          Set(NullValue, NumberValue(0))
        )
      }
      assertSuggest("""true""")(schema) { result =>
        assertEquals(
          result,
          Set(NullValue, NumberValue(0))
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
          Set(NumberValue(0), NumberValue(13), NumberValue(14))
        )
      }
      assertSuggest("""13""")(schema) { result =>
        assertEquals(
          result,
          Set(NumberValue(0), NumberValue(13), NumberValue(14))
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
          Set(BoolValue(true))
        )
      }
      assertSuggest("""13""")(schema) { result =>
        assertEquals(
          result,
          Set(BoolValue(true))
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
      assertSuggest("""{}""")(schema) { result =>
        assertEquals(
          result,
          Set(
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
          Set(
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
      assertSuggest("""{"foo": {}}""")(
        schema
      ) { result =>
        assertEquals(
          result,
          Set(
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
      assertSuggest("""{"toto": 13}""")(
        schema
      ) { result =>
        assertEquals(
          result,
          Set(
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
      assertSuggest("""[]""")(
        schema
      ) { result =>
        assertEquals(
          result,
          Set(
            ArrayValue(Seq()),
            ArrayValue(Seq(NumberValue(0)))
          )
        )
      }
    }
  }

  test("suggest at inside object") {
    withSchema(totoObjectSchema) { schema =>
      assertSuggest(
        """{"toto": 13}""",
        Pointer.empty / "toto"
      )(
        schema
      ) { result =>
        assertEquals(
          result,
          Set(
            NumberValue(0)
          )
        )
      }
    }
  }

  test("suggest item inside array") {
    withSchema(numberArraySchema) { schema =>
      assertSuggest(
        """[ 13, 14 ]""",
        Pointer.empty / 1
      )(
        schema
      ) { result =>
        assertEquals(
          result,
          Set(
            NumberValue(0)
          )
        )
      }
    }
  }

  test("suggest keys for schema writing") {
    assertSuggestForSchema(
      """{}""",
      Pointer.empty.insideKey
    ) { suggestions =>
      val suggestedKeys  = suggestions.flatMap(Value.asString).toSet
      val deprecatedKeys = Vocabulary.deprecatedKeywords
      val actual         = suggestedKeys -- deprecatedKeys

      val availableKeys = Vocabulary.specDialect().keywords.keys.toSet
      // see frawa.typedjson.keywords.Vocabulary.metaDataKeywords
      val todoKeywords = Set(
        "deprecated",
        "readOnly",
        "writeOnly",
        "examples"
      )

      val notSuggested = availableKeys -- actual
      assertEquals(notSuggested, Set.empty[String], "not suggested known keywords")

      val unknown = actual -- (availableKeys ++ todoKeywords)
      assertEquals(unknown, Set.empty[String], "suggested unknown keywords")
    }
  }

  private def assertSuggestForSchema(json: String, at: Pointer)(f: Set[Value] => Unit): Unit =
    val resolver     = MetaSchemas.lazyResolver
    val base         = MetaSchemas.draft202012
    val Some(schema) = resolver(base.resolve("schema")): @unchecked

    def factory(at: Pointer): EvaluatorFactory[SchemaValue, SuggestionOutput] =
      EvaluatorFactory.make(SuggestionProcessing(at), None, Some(resolver)).mapResult(assertNoIgnoredKeywords)

    given EvaluatorFactory[SchemaValue, SuggestionOutput] = factory(at)
    assertResult[SuggestionOutput](json)(schema) { result =>
      f(result.output.map(_.suggestions).getOrElse(Set()))
    }
