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
      checked = processor(InnerValue(value))
      _       = assertEquals(processor.validation.ignoredKeywords, Set.empty[String], "new keywords")
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

  test("minItems") {
    withSchema(
      """|{"minItems": 3
         |}""".stripMargin
    ) { schema =>
      validateJson(schema)("""[1,2]""") { checked =>
        assertEquals(
          checked.results,
          Seq(
            ValidationResult(
              Seq(
                WithPointer(MinItemsMismatch(3))
              )
            )
          )
        )
      }
      validateJson(schema)("""[2,3,4]""") { checked =>
        assert(checked.valid)
      }
    }
  }

  test("maxItems") {
    withSchema(
      """|{"maxItems": 2
         |}""".stripMargin
    ) { schema =>
      validateJson(schema)("""[1,2,3]""") { checked =>
        assertEquals(
          checked.results,
          Seq(
            ValidationResult(
              Seq(
                WithPointer(MaxItemsMismatch(2))
              )
            )
          )
        )
      }
      validateJson(schema)("""[2,3]""") { checked =>
        assert(checked.valid)
      }
    }
  }

  test("uniqueItems") {
    withSchema(
      """|{"uniqueItems": true
         |}""".stripMargin
    ) { schema =>
      validateJson(schema)("""[1,1]""") { checked =>
        assertEquals(
          checked.results,
          Seq(
            ValidationResult(
              Seq(
                WithPointer(ItemsNotUnique())
              )
            )
          )
        )
      }
      validateJson(schema)("""[13]""") { checked =>
        assert(checked.valid)
      }
    }
  }

  test("maxProperties") {
    withSchema(
      """|{"maxProperties": 2
         |}""".stripMargin
    ) { schema =>
      validateJson(schema)("""{"gnu": 1, "bar": 2, "foo": 3}""") { checked =>
        assertEquals(
          checked.results,
          Seq(
            ValidationResult(
              Seq(
                WithPointer(MaxPropertiesMismatch(2))
              )
            )
          )
        )
      }
      validateJson(schema)("""{"bar": 2, "foo": 3}""") { checked =>
        assert(checked.valid)
      }
    }
  }

  test("minProperties") {
    withSchema(
      """|{"minProperties": 3
         |}""".stripMargin
    ) { schema =>
      validateJson(schema)("""{"bar": 2, "foo": 3}""") { checked =>
        assertEquals(
          checked.results,
          Seq(
            ValidationResult(
              Seq(
                WithPointer(MinPropertiesMismatch(3))
              )
            )
          )
        )
      }
      validateJson(schema)("""{"gnu": 1, "bar": 2, "foo": 3}""") { checked =>
        assert(checked.valid)
      }
    }
  }

  test("required") {
    withSchema(
      """|{"required": ["bar", "foo"]
         |}""".stripMargin
    ) { schema =>
      validateJson(schema)("""{"gnu": 1, "bar": 2}""") { checked =>
        assertEquals(
          checked.results,
          Seq(
            ValidationResult(
              Seq(
                WithPointer(MissingRequiredProperties(Seq("foo")))
              )
            )
          )
        )
      }
      validateJson(schema)("""{"bar": 2, "foo": 3}""") { checked =>
        assert(checked.valid)
      }
    }
  }

  test("dependentRequired") {
    withSchema(
      """|{"dependentRequired": {"foo": ["bar", "gnu"]}
         |}""".stripMargin
    ) { schema =>
      validateJson(schema)("""{"foo": 1, "bar": 2}""") { checked =>
        assertEquals(
          checked.results,
          Seq(
            ValidationResult(
              Seq(
                WithPointer(DependentRequiredMissing(Map("foo" -> Seq("gnu"))))
              )
            )
          )
        )
      }
      validateJson(schema)("""{"foo": 1, "bar": 2, "gnu": 3}""") { checked =>
        assert(checked.valid)
      }
    }
  }

  test("dependentSchemas") {
    withSchema(
      """|{"dependentSchemas": {"foo": true, "gnu": false}}
         |}""".stripMargin
    ) { schema =>
      validateJson(schema)("""{"gnu": 1}""") { checked =>
        assertEquals(
          checked.results,
          Seq(
            ValidationResult(
              Seq(
                WithPointer(FalseSchemaReason())
              )
            )
          )
        )
      }
      validateJson(schema)("""{"foo": 1}""") { checked =>
        assert(checked.valid)
      }
    }
  }

  test("prefixItems") {
    withSchema(
      """|{"prefixItems": [{"type": "number"}, {"type": "string"}]
         |}""".stripMargin
    ) { schema =>
      validateJson(schema)("""["gnu"]""") { checked =>
        assertEquals(
          checked.results,
          Seq(
            ValidationResult(
              Seq(
                WithPointer(TypeMismatch("number"), Pointer.empty / 0)
              )
            )
          )
        )
      }
      validateJson(schema)("""[13, "foo", true]""") { checked =>
        assert(checked.valid)
      }
    }
  }

  test("prefixItems and itmes") {
    withSchema(
      """|{"prefixItems": [{"type": "number"}, {"type": "string"}],
         |"items": {"type": "boolean"}
         |}""".stripMargin
    ) { schema =>
      validateJson(schema)("""[13, "gnu", "boom"]""") { checked =>
        assertEquals(
          checked.results,
          Seq(
            ValidationResult(
              Seq(
                WithPointer(TypeMismatch("boolean"), Pointer.empty / 2)
              )
            )
          )
        )
      }
      validateJson(schema)("""[13, "foo", true]""") { checked =>
        assert(checked.valid)
      }
    }
  }

  test("contains") {
    withSchema(
      """|{"contains": {"type": "number"}
         |}""".stripMargin
    ) { schema =>
      validateJson(schema)("""["gnu", true]""") { checked =>
        assertEquals(
          checked.results,
          Seq(
            ValidationResult(
              Seq(
                WithPointer(NotContains(0))
              )
            )
          )
        )
      }
      validateJson(schema)("""[13, "foo", true]""") { checked =>
        assert(checked.valid)
      }
      validateJson(schema)("""[13, 14, "foo", true]""") { checked =>
        assert(checked.valid)
      }
    }
  }

  test("minContains") {
    withSchema(
      """|{"contains": {"type": "number"},
         |"minContains": 2
         |}""".stripMargin
    ) { schema =>
      validateJson(schema)("""[13, "gnu", true]""") { checked =>
        assertEquals(
          checked.results,
          Seq(
            ValidationResult(
              Seq(
                WithPointer(NotContains(1))
              )
            )
          )
        )
      }
      validateJson(schema)("""[13, 14, "foo", true]""") { checked =>
        assert(checked.valid)
      }
    }
  }

  test("maxContains") {
    withSchema(
      """|{"contains": {"type": "number"},
         |"maxContains": 2
         |}""".stripMargin
    ) { schema =>
      validateJson(schema)("""[13, 14, 15, "gnu", true]""") { checked =>
        assertEquals(
          checked.results,
          Seq(
            ValidationResult(
              Seq(
                WithPointer(NotContains(3))
              )
            )
          )
        )
      }
      validateJson(schema)("""[]""") { checked =>
        assertEquals(
          checked.results,
          Seq(
            ValidationResult(
              Seq(
                WithPointer(NotContains(0))
              )
            )
          )
        )
      }
      validateJson(schema)("""[13, 14, "foo", true]""") { checked =>
        assert(checked.valid)
      }
    }
  }

  test("minContains without contains") {
    withSchema(
      """|{"minContains": 2
         |}""".stripMargin
    ) { schema =>
      validateJson(schema)("""[13, "gnu", true]""") { checked =>
        assert(checked.valid)
      }
      validateJson(schema)("""[13, 14, "foo", true]""") { checked =>
        assert(checked.valid)
      }
    }
  }

  test("maxContains without contains") {
    withSchema(
      """|{"maxContains": 2
         |}""".stripMargin
    ) { schema =>
      validateJson(schema)("""[13, "gnu", true]""") { checked =>
        assert(checked.valid)
      }
      validateJson(schema)("""[13, 14, "foo", true]""") { checked =>
        assert(checked.valid)
      }
    }
  }

  test("patternProperties") {
    withSchema(
      """|{"patternProperties": { "^f": {} },
         |"additionalProperties": false
         |}""".stripMargin
    ) { schema =>
      validateJson(schema)("""{"gnu": 13, "bar": true}""") { checked =>
        assertEquals(
          checked.results,
          Seq(
            ValidationResult(
              Seq(
                WithPointer(FalseSchemaReason(), Pointer.empty / "gnu"),
                WithPointer(FalseSchemaReason(), Pointer.empty / "bar")
              )
            )
          )
        )
      }
      validateJson(schema)("""{"foo": "ok"}]""") { checked =>
        assertEquals(checked.results, Seq())
        assert(checked.valid)
      }
    }
  }
}
