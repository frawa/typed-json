package frawa.typedjson.schema

import munit.FunSuite
import frawa.typedjson.parser.ZioParser
import frawa.typedjson.parser.Parser

class QuickfixTest extends FunSuite {
  implicit val zioParser    = new ZioParser();
  implicit val schemaParser = SchemaValueDecoder;

  private def testQuickfix(text: String)(f: Quickfix => Unit) {
    val withQuickfix = for {
      schema <- SchemaParser(text)
      quickfix = Quickfix(schema)
    } yield {
      f(quickfix)
    }
    withQuickfix.swap
      .map(message => fail("no quickfix", clues(clue(message))))
      .swap
  }

  private def assertQuickfix(text: String, quickfix: Quickfix)(f: QuickfixResult => Unit) = {
    val withParsed = for {
      value <- Parser(text)
      result = Quickfix.fixes(quickfix)(value)
    } yield {
      f(result)
    }
    withParsed.swap
      .map(message => fail("parsing failed", clues(clue(message))))
      .swap
  }

  test("object add missing property") {
    testQuickfix("""{
                   |"$id": "testme",
                   |"type": "object", 
                   |"properties": { 
                   |  "toto": { "type": "number" },
                   |  "titi": { "type": "string" }
                   |} 
                   |}
                   |""".stripMargin) { quickfix =>
      assertQuickfix(
        """{
          |"toto": 13
          |}
          |""".stripMargin,
        quickfix
      ) { result =>
        assertEquals(result.fixes, Seq(AddProperty(Pointer.empty, "titi")))
      }
    }
  }
}
