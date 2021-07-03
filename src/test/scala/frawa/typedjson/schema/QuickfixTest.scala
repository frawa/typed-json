package frawa.typedjson.schema

import munit.FunSuite
import frawa.typedjson.parser.ZioParser
import frawa.typedjson.parser.Parser

class QuickfixTest extends FunSuite {
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

  private def assertQuickfix(text: String)(schema: Schema)(f: QuickfixResult => Unit) = {
    val withParsed = for {
      value <- Parser(text)
      result = Quickfix.fixes(schema)(value)
    } yield {
      f(result)
    }
    withParsed.swap
      .map(message => fail("parsing failed", clues(clue(message))))
      .swap
  }

  test("object add missing property") {
    testSchema("""{
                 |"$id": "testme",
                 |"type": "object", 
                 |"properties": { 
                 |  "toto": { "type": "number" },
                 |  "titi": { "type": "string" }
                 |} 
                 |}
                 |""".stripMargin) { schema =>
      assertQuickfix("""{
                       |"toto": 13
                       |}
                       |""".stripMargin)(
        schema
      ) { result =>
        assertEquals(result, QuickfixResultFixes(Seq(AddProperties(Pointer.empty, Seq("titi")))))
      }
    }
  }

  test("object add deep missing property") {
    testSchema("""{
                 |"$id": "testme",
                 |"type": "object", 
                 |"properties": { 
                 |  "toto": { "type": "number" },
                 |  "titi": { "type": "string" },
                 |  "foo": { 
                 |    "type": "object",
                 |    "properties": {
                 |      "bar": { "type": "boolean" }
                 |    } 
                 |  }
                 |} 
                 |}
                 |""".stripMargin) { schema =>
      assertQuickfix("""{
                       |"toto": 13,
                       |"foo": {}
                       |}
                       |""".stripMargin)(
        schema
      ) { result =>
        assertEquals(
          result,
          QuickfixResultFixes(
            Seq(
              AddProperties(Pointer.empty / "foo", Seq("bar")),
              AddProperties(Pointer.empty, Seq("titi"))
            )
          )
        )
      }
    }
  }

  test("anyOf raises two groups") {
    testSchema("""{
                 |"$id": "testme",
                 |"anyOf": [{
                 |  "type": "object", 
                 |  "properties": { 
                 |    "toto": { "type": "number" }
                 |  }},{ 
                 |  "type": "object", 
                 |  "properties": { 
                 |    "titi": { "type": "string" }
                 |  }}
                 |]
                 |}
                 |""".stripMargin) { schema =>
      assertQuickfix("""{}""".stripMargin)(
        schema
      ) { result =>
        assertEquals(
          result,
          QuickfixResultFixes(
            Seq(
              QuickfixItemGroup(Seq(AddProperties(Pointer.empty, Seq("toto")))),
              QuickfixItemGroup(Seq(AddProperties(Pointer.empty, Seq("titi"))))
            )
          )
        )
      }
    }
  }

  test("null or bool") {
    testSchema("""{
                 |"$id": "testme",
                 |"type": ["null","boolean"]
                 |}""".stripMargin) { schema =>
      assertQuickfix("""13""")(schema) { result =>
        assertEquals(
          result,
          QuickfixResultEmpty
        )
      }
    }
  }

  test("enum") {
    testSchema("""{
                 |"$id": "testme",
                 |"type": "string",
                 |"enum": ["foo", "bar"]
                 |}""".stripMargin) { schema =>
      assertQuickfix(""""foo"""")(schema) { result =>
        assertEquals(result, QuickfixResultEmpty)
      }
      assertQuickfix(""""bar"""")(schema) { result =>
        assertEquals(result, QuickfixResultEmpty)
      }
      assertQuickfix("""13""")(schema) { result =>
        assertEquals(result, QuickfixResultEmpty)
      }
    }
  }

}
