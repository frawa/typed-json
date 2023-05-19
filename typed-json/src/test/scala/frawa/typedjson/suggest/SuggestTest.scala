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

package frawa.typedjson.suggest

import frawa.typedjson.keywords.{SchemaValue, Vocabulary}
import frawa.typedjson.meta.MetaSchemas
import frawa.typedjson.parser.*
import frawa.typedjson.parser.Value.*
import frawa.typedjson.pointer.Pointer
import frawa.typedjson.testutil.TestSchemas.{numberArraySchema, totoObjectSchema, totoRequiredObjectSchema}
import frawa.typedjson.testutil.TestUtil.{*, given}
import munit.FunSuite
import frawa.typedjson.output.SimpleOutput
import frawa.typedjson.eval.Eval
import frawa.typedjson.eval.CacheState.R
import frawa.typedjson.eval.Util.withCompiledSchemaValue
import frawa.typedjson.eval.Util.doApply
import frawa.typedjson.suggest.Suggest
import frawa.typedjson.suggest.SuggestOutput
import frawa.typedjson.output.OutputOps
import munit.Compare
import frawa.typedjson.eval.Util.vocabularyForTest
import frawa.typedjson.keywords.MetaKeyword

class SuggestTest extends FunSuite:

  // TODO avoid?
  given Compare[SuggestResult, SuggestResult] with
    def isEqual(obtained: SuggestResult, expected: SuggestResult): Boolean =
      obtained.suggestions.toSet.equals(expected.suggestions.toSet)

  given Compare[Suggest, Suggest] with
    def isEqual(obtained: Suggest, expected: Suggest): Boolean =
      (obtained, expected) match {
        case (Suggest.Values(vs1), Suggest.Values(vs2)) =>
          vs1.toSet.equals(vs2.toSet)
        case (Suggest.WithDoc(vs1, doc1), Suggest.WithDoc(vs2, doc2)) =>
          vs1.toSet.equals(vs2.toSet) && doc1.equals(doc2)
        case _ => false
      }

  private val vocabularyWithMeta = vocabularyForTest.map { voc =>
    voc.combine(Vocabulary.specVocabularies(Vocabulary.metaDataId))
  }

  private def assertSuggest(text: String, at: Pointer = Pointer.empty)(schema: SchemaValue)(
      f: SuggestResult => Unit
  ) =
    given OutputOps[SuggestOutput] = SuggestOutput.outputOps(at)
    val evalSuggest                = Eval[R, SuggestOutput]
    given Eval[R, SuggestOutput]   = evalSuggest
    val value                      = parseJsonValue(text)
    withCompiledSchemaValue(schema, vocabulary = vocabularyWithMeta) { fun =>
      val suggestFun = Suggest.suggestAt(at)(fun)
      val output     = doApply(fun, value)
      val result     = Suggest.suggestions(at, output)
      f(result)
    }

  private def assertSuggestForSchema(text: String, at: Pointer)(f: SuggestResult => Unit): Unit =
    val lazyResolver = MetaSchemas.lazyResolver
    val base         = MetaSchemas.draft202012
    val Some(schema) = lazyResolver(base.resolve("schema")): @unchecked

    given OutputOps[SuggestOutput] = SuggestOutput.outputOps(at)
    val evalSuggest                = Eval[R, SuggestOutput]
    given Eval[R, SuggestOutput]   = evalSuggest
    val value                      = parseJsonValue(text)
    withCompiledSchemaValue(schema, Some(lazyResolver), vocabularyWithMeta) { fun =>
      val suggestFun = Suggest.suggestAt(at)(fun)
      val output     = doApply(fun, value)
      val result     = Suggest.suggestions(at, output)
      f(result)
    }

  private def suggests(vs: Value*): SuggestResult =
    SuggestResult(Seq(Suggest.Values(vs)))
  private def suggestWith(doc: Suggest.Doc, vs: Value*): SuggestResult =
    SuggestResult(Seq(Suggest.WithDoc(vs, doc)))

  test("suggest one object per property") {
    withSchema(totoObjectSchema) { schema =>
      assertSuggest("""{"toto": 13}""")(
        schema
      ) { result =>
        assertEquals(
          result,
          suggests(
            ObjectValue(Map("titi" -> StringValue(""), "toto" -> NumberValue(0))),
            ObjectValue(Map())
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
          suggests(
            StringValue("titi"),
            StringValue("toto")
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
          suggests(
            ObjectValue(Map("foo" -> ObjectValue(Map("bar" -> NumberValue(0))))),
            ObjectValue(Map())
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
          suggests(
            ObjectValue(
              properties = Map(
                "foo" -> ObjectValue(
                  properties = Map(
                    "bar" -> NumberValue(
                      value = 0
                    )
                  )
                ),
                "gnu" -> ObjectValue(
                  properties = Map(
                    "toto" -> StringValue(
                      value = ""
                    )
                  )
                )
              )
            ),
            ObjectValue(Map())
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
          suggests(
            ObjectValue(
              Map(
                "foo" -> ObjectValue(
                  Map(
                    "bar" ->
                      NumberValue(0),
                    "gnu" ->
                      NumberValue(0)
                  )
                )
              )
            ),
            ObjectValue(
              properties = Map()
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
          suggests(NullValue, NumberValue(0))
        )
      }
      assertSuggest("""true""")(schema) { result =>
        assertEquals(
          result,
          suggests(NullValue, NumberValue(0))
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
          suggests(NumberValue(13), NumberValue(14), NumberValue(0))
        )
      }
      assertSuggest("""13""")(schema) { result =>
        assertEquals(
          result,
          suggests(NumberValue(13), NumberValue(14), NumberValue(0))
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
          suggests(BoolValue(true))
        )
      }
      assertSuggest("""13""")(schema) { result =>
        assertEquals(
          result,
          suggests(BoolValue(true))
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
          suggests(
            ObjectValue(
              properties = Map(
                "kind" -> StringValue(
                  value = "first"
                )
              )
            ),
            ObjectValue(
              properties = Map()
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
          suggests(
            ObjectValue(
              properties = Map(
                "kind" -> StringValue(
                  value = "first"
                )
              )
            ),
            ObjectValue(
              properties = Map()
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
          suggests(
            ObjectValue(
              Map(
                "foo" -> ObjectValue(
                  Map(
                    "bar" -> NumberValue(14),
                    "gnu" -> StringValue("titi")
                  )
                )
              )
            ),
            ObjectValue(Map())
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
          suggests(
            ObjectValue(properties =
              Map(
                "gnu"  -> BoolValue(true),
                "titi" -> StringValue(""),
                "toto" -> NumberValue(0)
              )
            ),
            ObjectValue(Map()),
            ObjectValue(
              properties = Map(
                "toto" -> NullValue,
                "gnu"  -> NullValue
              )
            )
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
          suggests(
            ArrayValue(Seq(NumberValue(0))),
            ArrayValue(Seq())
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
          suggests(
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
          suggests(
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
    ) { result =>
      val suggestedKeys  = result.suggestions.flatMap(_.values).flatMap(Value.asString).toSet
      val deprecatedKeys = Vocabulary.deprecatedKeywords
      val actual         = suggestedKeys -- deprecatedKeys

      val availableKeys = Vocabulary.specDialect().keywords.keys.toSet

      val notSuggested = availableKeys -- actual
      assertEquals(notSuggested, Set.empty[String], "not suggested known keywords")

      val unknown = actual -- availableKeys
      assertEquals(unknown, Set.empty[String], "suggested unknown keywords")
    }
  }

  test("meta default") {
    withSchema("""{
                 |"default": 13,
                 |"type": "number"
                 |}""".stripMargin) { schema =>
      assertSuggest("""13""")(schema) { result =>
        assertEquals(
          result,
          suggests(
            NumberValue(13),
            NumberValue(0)
          )
        )
      }
    }
  }

  test("meta examples") {
    withSchema("""{
                 |"examples": [13,14],
                 |"type": "number"
                 |}""".stripMargin) { schema =>
      assertSuggest("""13""")(schema) { result =>
        assertEquals(
          result,
          suggests(
            NumberValue(13),
            NumberValue(14),
            NumberValue(0)
          )
        )
      }
    }
  }

  test("first with title".ignore) {
    withSchema("""{
                 |"title": "My Title",
                 |"type": "number"
                 |}""".stripMargin) { schema =>
      assertSuggest("""13""")(schema) { result =>
        assertEquals(
          result,
          suggestWith(
            Suggest.Doc(title = Some("My Title")),
            NullValue,
            NumberValue(0)
          )
        )
      }
    }
  }

  test("suggest values for schema writing".ignore) {
    assertSuggestForSchema(
      """{}""",
      Pointer.empty
    ) { result =>
      // TODO assert different meta schema titles for various suggested values
      // println(s"FW ${munitPrint(result)}")
    }
  }
