package frawa.typedjson.schema

import munit.FunSuite
import frawa.typedjson.parser.ZioParser
import frawa.typedjson.parser.Parser

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

  private def assertSuggest(text: String, at: Pointer)(schema: Schema)(f: SuggestionResult => Unit) = {
    val withParsed = for {
      value <- Parser(text)
      result = Suggestion.suggestions(schema)(value, at)
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
          |""".stripMargin,
        Pointer.empty
      )(
        schema
      ) { result =>
        assertEquals(result, SuggestionResult(Seq("titi")))
      }
    }
  }

  test("suggest at deep pointer") {
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
          |""".stripMargin,
        Pointer.empty / "foo"
      )(
        schema
      ) { result =>
        assertEquals(result, SuggestionResult(Seq("bar")))
      }
    }
  }

  test("suggest at another pointer".ignore) {
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
          |""".stripMargin,
        Pointer.empty / "foo"
      )(
        schema
      ) { result =>
        assertEquals(result, SuggestionResult(Seq("bar")))
      }
      assertSuggest(
        """{
          |"foo": { "bar": 13 },
          |"gnu": {}
          |}
          |""".stripMargin,
        Pointer.empty / "gnu"
      )(
        schema
      ) { result =>
        assertEquals(result, SuggestionResult(Seq("toto")))
      }
    }
  }
// TODO
// - suggest at given pointer
// - suggest at non-existing pointer
// - suggest existing property
// - suggest contant property value

}
