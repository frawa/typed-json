package frawa.typedjson.schema

import frawa.typedjson.parser.Parser
import frawa.typedjson.parser.{ObjectValue, NumberValue, StringValue}
import frawa.typedjson.parser.ZioParser
import munit.FunSuite

class MultiEvalTest extends FunSuite {
  implicit val zioParser    = new ZioParser();
  implicit val schemaParser = SchemaValueDecoder;

  private def withEvaluator[R](text: String)(f: Evaluator => Unit) = {
    val withSchema = for {
      schema <- SchemaParser(text)
      evaluator = Evaluator(schema)
    } yield {
      f(evaluator)
    }
    withSchema.swap
      .map(message => fail("no schema", clues(clue(message))))
      .swap
  }

  private def assertResult[R](text: String)(evaluator: Evaluator)(calc: ResultCalculator[R])(f: R => Unit) = {
    val withParsed = for {
      value <- Parser(text)
      result = evaluator(value)(calc)
    } yield {
      f(result)
    }
    withParsed.swap
      .map(message => fail("parsing failed", clues(clue(message))))
      .swap
  }

  private def assertValidate(text: String)(evaluator: Evaluator)(f: ValidationResult => Unit) = {
    assertResult(text)(evaluator)(ValidationResultCalculator)(f)
  }

  private def assertQuickfix(text: String)(evaluator: Evaluator)(f: QuickfixResult => Unit) = {
    assertResult(text)(evaluator)(QuickfixResultCalculator)(f)
  }

  private def assertSuggestion(text: String)(evaluator: Evaluator)(f: SuggestionResult => Unit) = {
    assertResult(text)(evaluator)(SuggestionResultCalculator)(f)
  }

  test("first") {
    withEvaluator("""{
                    |"$id": "testme",
                    |"type": "object", 
                    |"properties": { 
                    |  "toto": { "type": "number" },
                    |  "titi": { "type": "string" }
                    |} 
                    |}
                    |""".stripMargin) { evaluator =>
      assertValidate("""true""")(evaluator) { result =>
        assertEquals(
          result.errors,
          Seq(
            WithPointer(
              result = TypeMismatch(
                expected = ObjectSchema(
                  properties = Map(
                    "toto" -> NumberSchema,
                    "titi" -> StringSchema
                  )
                )
              ),
              pointer = Pointer(
                segments = Nil
              )
            )
          )
        )
      }
      assertQuickfix("""true""")(evaluator) { result =>
        assertEquals(result, QuickfixResultEmpty)
      }
      assertSuggestion("""true""")(evaluator) { result =>
        assertEquals(
          result,
          SuggestionResult(
            Seq(
              ObjectValue(
                properties = Map()
              )
            )
          )
        )
      }
      assertSuggestion("""{}""")(evaluator) { result =>
        assertEquals(
          result,
          SuggestionResult(
            Seq(
              ObjectValue(
                properties = Map(
                  "toto" -> NumberValue(
                    value = 0
                  )
                )
              ),
              ObjectValue(
                Map(
                  "titi" -> StringValue(
                    value = ""
                  )
                )
              )
            )
          )
        )
      }
    }
  }
}
