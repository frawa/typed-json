package frawa.typedjson.schema

import munit.FunSuite
import frawa.typedjson.parser.ZioParser
import frawa.typedjson.parser.Parser
import frawa.typedjson.parser.{ObjectValue, NullValue, NumberValue, StringValue}
import frawa.typedjson.parser.BoolValue

class SuggestTest extends FunSuite {
  implicit val zioParser    = new ZioParser();
  implicit val schemaParser = SchemaValueDecoder;

  private def testSchema(text: String)(f: Schema => Unit) {
    val withSchema = for {
      schema <- SchemaParser(text)
    } yield {
      f(schema)
    }
    withSchema.swap
      .map(message => fail("no schema", clues(clue(message))))
      .swap
  }

  private def assertSuggest(text: String)(schema: Schema)(f: SuggestionResult => Unit) = {
    val withParsed = for {
      value <- Parser(text)
      result = Suggestion.suggestions(schema)(value)
    } yield {
      f(result)
    }
    withParsed.swap
      .map(message => fail("parsing failed", clues(clue(message))))
      .swap
  }

  test("suggest missing property") {
    testSchema("""{
                 |"$id": "testme",
                 |"type": "object", 
                 |"properties": { 
                 |  "toto": { "type": "number" },
                 |  "titi": { "type": "string" }
                 |} 
                 |}
                 |""".stripMargin) { schema =>
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
          SuggestionResult(
            Seq(
              ObjectValue(Map("toto" -> NumberValue(0))),
              ObjectValue(Map("titi" -> StringValue("")))
            )
          )
        )
      }
    }
  }

  test("suggest deep") {
    testSchema("""{
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
          SuggestionResult(Seq(ObjectValue(Map("foo" -> ObjectValue(Map("bar" -> NumberValue(0)))))))
        )
      }
    }
  }

  test("suggest several values") {
    testSchema("""{
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
          SuggestionResult(
            Seq(
              ObjectValue(Map("foo" -> ObjectValue(Map("bar" -> NumberValue(0))))),
              ObjectValue(Map("gnu" -> ObjectValue(Map())))
            )
          )
        )
      }
      assertSuggest(
        """{
          |"foo": { "bar": 13 },
          |"gnu": {}
          |}
          |""".stripMargin
      )(
        schema
      ) { result =>
        assertEquals(
          result,
          SuggestionResult(
            Seq(
              ObjectValue(Map("foo" -> ObjectValue(Map("bar" -> NumberValue(0))))),
              ObjectValue(Map("gnu" -> ObjectValue(Map("toto" -> StringValue("")))))
            )
          )
        )
      }
    }
  }

  test("suggestions for several properties") {
    testSchema("""{
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
          SuggestionResult(
            Seq(
              ObjectValue(Map("foo" -> ObjectValue(Map("bar" -> NumberValue(0), "gnu" -> NumberValue(0)))))
            )
          )
        )
      }
      assertSuggest(
        """{
          |"foo": { "bar": 13 }
          |}
          |""".stripMargin
      )(
        schema
      ) { result =>
        assertEquals(
          result,
          SuggestionResult(
            Seq(
              ObjectValue(Map("foo" -> ObjectValue(Map("bar" -> NumberValue(0))))),
              ObjectValue(Map("foo" -> ObjectValue(Map("gnu" -> NumberValue(0)))))
            )
          )
        )
      }
    }
  }

  test("null or number") {
    testSchema("""{
                 |"$id": "testme",
                 |"type": ["null","number"]
                 |}""".stripMargin) { schema =>
      assertSuggest("""13""")(schema) { result =>
        assertEquals(
          result,
          SuggestionResult(Seq(NullValue, NumberValue(0)))
        )
      }
      assertSuggest("""true""")(schema) { result =>
        assertEquals(
          result,
          SuggestionResult(Seq(NullValue, NumberValue(0)))
        )
      }
    }
  }

  test("enum") {
    testSchema("""{
                 |"$id": "testme",
                 |"type": "number",
                 |"enum": [13, 14]                
                 |}""".stripMargin) { schema =>
      assertSuggest("""true""")(schema) { result =>
        assertEquals(
          result,
          SuggestionResult(Seq(NumberValue(13), NumberValue(14)))
        )
      }
      assertSuggest("""13""")(schema) { result =>
        assertEquals(
          result,
          SuggestionResult(Seq(NumberValue(13), NumberValue(14)))
        )
      }
    }
  }

  test("const") {
    testSchema("""{
                 |"$id": "testme",
                 |"type": "boolean",
                 |"const": true               
                 |}""".stripMargin) { schema =>
      assertSuggest("""true""")(schema) { result =>
        assertEquals(
          result,
          SuggestionResult(Seq(BoolValue(true)))
        )
      }
      assertSuggest("""13""")(schema) { result =>
        assertEquals(
          result,
          SuggestionResult(Seq(BoolValue(true)))
        )
      }
    }
  }

  test("discriminator") {
    testSchema("""{
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
          SuggestionResult(
            Seq(
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
        )
      }
      assertSuggest("""{"kind":"first"}""")(schema) { result =>
        assertEquals(
          result,
          SuggestionResult(
            Seq(
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
        )
      }
    }
  }

// TODO
// - suggest array (next item?)

}
