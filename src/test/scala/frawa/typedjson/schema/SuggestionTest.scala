package frawa.typedjson.schema

import munit.FunSuite
import frawa.typedjson.parser.ZioParser
import frawa.typedjson.parser.Parser
import frawa.typedjson.parser.{ObjectValue, NullValue, NumberValue, StringValue}
import frawa.typedjson.parser.BoolValue
import TestUtil._
import TestSchemas._

class SuggestTest extends FunSuite {
  implicit val zioParser = new ZioParser();

  private def assertSuggest(text: String)(schema: SchemaValue)(
      f: SuggestionResult => Unit
  ) = {
    val withParsed = for {
      value     <- Parser(text)
      processor <- Processor(schema)(new SuggestionChecker())
      result = processor.process(value)
    } yield {
      f(result)
    }
    withParsed.swap
      .map(message => fail("parsing failed", clues(clue(message))))
      .swap
  }

  test("suggest missing property".only) {
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
          SuggestionResult(
            Seq(
              ObjectValue(Map()),
              ObjectValue(Map("toto" -> NumberValue(0))),
              ObjectValue(Map("titi" -> StringValue("")))
            )
          )
        )
      }
    }
  }

  test("suggest deep".only) {
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
          SuggestionResult(
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
        )
      }
    }
  }

  test("suggest several values".only) {
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
          SuggestionResult(
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
        )
      }
    }
  }

  test("suggestions for several properties".only) {
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
          SuggestionResult(
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
        )
      }
    }
  }

  test("null or number".only) {
    withSchema("""{
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

  test("enum".only) {
    withSchema("""{
                 |"$id": "testme",
                 |"type": "number",
                 |"enum": [13, 14]                
                 |}""".stripMargin) { schema =>
      assertSuggest("""true""")(schema) { result =>
        assertEquals(
          result,
          SuggestionResult(Seq(NumberValue(0), NumberValue(13), NumberValue(14)))
        )
      }
      assertSuggest("""13""")(schema) { result =>
        assertEquals(
          result,
          SuggestionResult(Seq(NumberValue(0), NumberValue(13), NumberValue(14)))
        )
      }
    }
  }

  test("const".only) {
    withSchema("""{
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
          SuggestionResult(
            Seq(
              ObjectValue(Map("foo" -> ObjectValue(Map("bar" -> NumberValue(13))))),
              ObjectValue(Map("foo" -> ObjectValue(Map("bar" -> NumberValue(14))))),
              ObjectValue(Map("foo" -> ObjectValue(Map("gnu" -> StringValue("toto"))))),
              ObjectValue(Map("foo" -> ObjectValue(Map("gnu" -> StringValue("titi")))))
            )
          )
        )
      }
    }
  }

// TODO
// - suggest array (next item?)

}
