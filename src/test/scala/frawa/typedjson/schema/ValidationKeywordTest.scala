package frawa.typedjson.schemaSpec

import frawa.typedjson.parser.ZioParser
import munit.FunSuite
import frawa.typedjson.schema.SchemaValue
import frawa.typedjson.schema.Processor
import frawa.typedjson.schema.ValidationChecker
import frawa.typedjson.schema.Checked
import frawa.typedjson.schema.ValidationResult
import frawa.typedjson.schema.InnerValue
import frawa.typedjson.schema.TestUtil._
import frawa.typedjson.schema._

class ValidationKeywordTest extends FunSuite {
  implicit val zioParser = new ZioParser()

  def validateJson(schema: SchemaValue)(jsonText: String)(f: Checked[ValidationResult] => Unit) = {
    val value = parseJsonValue(jsonText)
    val result = for {
      processor <- Processor(schema)(ValidationChecker())
      checked = processor.process(InnerValue(value))
      _       = assertEquals(processor.ignoredKeywords, Set.empty[String])
    } yield {
      f(checked)
    }
    result.swap
      .map(message => fail("validating spec failed", clues(clue(message))))
      .swap
  }

  test("multipleOf") {
    withSchema(
      """|{"type": "number",
         |"multipleOf": 2
         |}""".stripMargin
    ) { schema =>
      validateJson(schema)("""13""") { checked =>
        assertEquals(
          checked.results,
          Seq(
            ValidationResult(
              Seq(
                WithPointer(NotMultipleOf(2))
              )
            )
          )
        )
      }
      validateJson(schema)("""12""") { checked =>
        assert(checked.valid)
      }
    }
  }

  test("maximum") {
    withSchema(
      """|{"type": "number",
         |"maximum": 13
         |}""".stripMargin
    ) { schema =>
      validateJson(schema)("""1313""") { checked =>
        assertEquals(
          checked.results,
          Seq(
            ValidationResult(
              Seq(
                WithPointer(MaximumMismatch(13, false))
              )
            )
          )
        )
      }
      validateJson(schema)("""12""") { checked =>
        assert(checked.valid)
      }
    }
  }

  test("exclusiveMaximum") {
    withSchema(
      """|{"type": "number",
         |"exclusiveMaximum": 13
         |}""".stripMargin
    ) { schema =>
      validateJson(schema)("""13""") { checked =>
        assertEquals(
          checked.results,
          Seq(
            ValidationResult(
              Seq(
                WithPointer(MaximumMismatch(13, true))
              )
            )
          )
        )
      }
      validateJson(schema)("""12""") { checked =>
        assert(checked.valid)
      }
    }
  }

  test("miniimum") {
    withSchema(
      """|{"type": "number",
         |"minimum": 13
         |}""".stripMargin
    ) { schema =>
      validateJson(schema)("""12""") { checked =>
        assertEquals(
          checked.results,
          Seq(
            ValidationResult(
              Seq(
                WithPointer(MinimumMismatch(13, false))
              )
            )
          )
        )
      }
      validateJson(schema)("""1313""") { checked =>
        assert(checked.valid)
      }
    }
  }

  test("exclusiveMinimum") {
    withSchema(
      """|{"type": "number",
         |"exclusiveMinimum": 13
         |}""".stripMargin
    ) { schema =>
      validateJson(schema)("""13""") { checked =>
        assertEquals(
          checked.results,
          Seq(
            ValidationResult(
              Seq(
                WithPointer(MinimumMismatch(13, true))
              )
            )
          )
        )
      }
      validateJson(schema)("""14""") { checked =>
        assert(checked.valid)
      }
    }
  }
}
