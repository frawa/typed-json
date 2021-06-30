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
        assertEquals(result, QuickfixResultFixes(Seq(AddProperty(Pointer.empty, "titi"))))
      }
    }
  }
}
