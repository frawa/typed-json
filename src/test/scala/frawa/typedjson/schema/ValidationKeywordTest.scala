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
      _       = assertEquals(processor.ignoredKeywords, Set.empty[String], "new keywords")
    } yield {
      f(checked)
    }
    result.swap
      .map(message => fail("validating spec failed", clues(clue(message))))
      .swap
  }

  test("multipleOf") {
    withSchema(
      """|{"multipleOf": 2
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
      """|{"maximum": 13
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
      """|{"exclusiveMaximum": 13
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

  test("minimum") {
    withSchema(
      """|{"minimum": 13
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
      """|{"exclusiveMinimum": 13
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

  test("maxLength") {
    withSchema(
      """|{"maxLength": 3
         |}""".stripMargin
    ) { schema =>
      validateJson(schema)(""""toto"""") { checked =>
        assertEquals(
          checked.results,
          Seq(
            ValidationResult(
              Seq(
                WithPointer(MaxLengthMismatch(3))
              )
            )
          )
        )
      }
      validateJson(schema)(""""bar"""") { checked =>
        assert(checked.valid)
      }
    }
  }

  test("minLength") {
    withSchema(
      """|{"minLength": 4
         |}""".stripMargin
    ) { schema =>
      validateJson(schema)(""""bar"""") { checked =>
        assertEquals(
          checked.results,
          Seq(
            ValidationResult(
              Seq(
                WithPointer(MinLengthMismatch(4))
              )
            )
          )
        )
      }
      validateJson(schema)(""""toto"""") { checked =>
        assert(checked.valid)
      }
    }
  }

  test("pattern") {
    withSchema(
      """|{"pattern": "foo\\d\\d"
         |}""".stripMargin
    ) { schema =>
      validateJson(schema)(""""foo"""") { checked =>
        assertEquals(
          checked.results,
          Seq(
            ValidationResult(
              Seq(
                WithPointer(PatternMismatch("foo\\d\\d"))
              )
            )
          )
        )
      }
      validateJson(schema)(""""foo13"""") { checked =>
        assert(checked.valid)
      }
    }
  }
}
