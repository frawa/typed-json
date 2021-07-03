package frawa.typedjson.schema

import munit.FunSuite
import frawa.typedjson.parser.ZioParser
import frawa.typedjson.parser.Parser
import frawa.typedjson.parser.{ObjectValue, NullValue, NumberValue, StringValue}

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
        assertEquals(result, SuggestionResult(Seq(ObjectValue(Map("titi" -> StringValue(""))))))
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
          SuggestionResult(Seq(ObjectValue(Map("gnu" -> ObjectValue(Map("toto" -> StringValue("")))))))
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
          SuggestionResult(Seq(ObjectValue(Map("foo" -> ObjectValue(Map("gnu" -> NumberValue(0)))))))
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
          SuggestionResult(Seq(NullValue))
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

  test("enumr") {
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
          // TODO suggest other enum values
          // SuggestionResult(Seq(NumberValue(13), NumberValue(14)))
          SuggestionResult(Seq())
        )
      }
    }
  }

// TODO
// - suggest array (next item?)
// - suggest existing property
// - suggest all enum values

}
