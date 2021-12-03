/*
 * Copyright 2021 Frank Wagner
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package frawa.typedjson.validation

import frawa.typedjson
import frawa.typedjson.parser.ZioParser
import frawa.typedjson.processor.SchemaProblems.MissingReference
import frawa.typedjson.processor._
import frawa.typedjson.testutil.ProcessorConversion
import frawa.typedjson.testutil.TestUtil._
import munit.FunSuite

class ValidationKeywordTest extends FunSuite {
  implicit val zioParser: ZioParser = new ZioParser()

  private val vocabularyForTest = Vocabulary
    .dialect(Map(Vocabulary.coreId -> true, Vocabulary.validationId -> true, Vocabulary.applicatorId -> true))
    .swap
    .map(problems => throw new IllegalStateException(problems.dump()))
    .swap
    .toOption

  private implicit val toProcessor1: ProcessorConversion[SchemaValue, ValidationResult] =
    ProcessorConversion.toProcessor(ValidationEval(), vocabularyForTest).mapResult(assertNoIgnoredKeywords)

  def validateJson(
      schema: SchemaValue
  )(jsonText: String)(f: Result[ValidationResult] => Unit): Either[Nothing, Unit] = {
    assertResult(jsonText)(schema)(f)
  }

  test("multipleOf") {
    withSchema(
      """|{"multipleOf": 2
         |}""".stripMargin
    ) { schema =>
      validateJson(schema)("""13""") { result =>
        assertEquals(
          result.results,
          Seq(
            ValidationResult(
              Seq(
                WithPointer(NotMultipleOf(2))
              )
            )
          )
        )
      }
      validateJson(schema)("""12""") { result =>
        assert(result.valid)
        assertEquals(result.annotations, Seq())
      }
    }
  }

  test("maximum") {
    withSchema(
      """|{"maximum": 13
         |}""".stripMargin
    ) { schema =>
      validateJson(schema)("""1313""") { result =>
        assertEquals(
          result.results,
          Seq(
            ValidationResult(
              Seq(
                WithPointer(MaximumMismatch(13, false))
              )
            )
          )
        )
      }
      validateJson(schema)("""12""") { result =>
        assert(result.valid)
        assertEquals(result.annotations, Seq())
      }
    }
  }

  test("exclusiveMaximum") {
    withSchema(
      """|{"exclusiveMaximum": 13
         |}""".stripMargin
    ) { schema =>
      validateJson(schema)("""13""") { result =>
        assertEquals(
          result.results,
          Seq(
            ValidationResult(
              Seq(
                WithPointer(MaximumMismatch(13, true))
              )
            )
          )
        )
      }
      validateJson(schema)("""12""") { result =>
        assert(result.valid)
        assertEquals(result.annotations, Seq())
      }
    }
  }

  test("minimum") {
    withSchema(
      """|{"minimum": 13
         |}""".stripMargin
    ) { schema =>
      validateJson(schema)("""12""") { result =>
        assertEquals(
          result.results,
          Seq(
            ValidationResult(
              Seq(
                WithPointer(MinimumMismatch(13, false))
              )
            )
          )
        )
      }
      validateJson(schema)("""1313""") { result =>
        assert(result.valid)
        assertEquals(result.annotations, Seq())
      }
    }
  }

  test("exclusiveMinimum") {
    withSchema(
      """|{"exclusiveMinimum": 13
         |}""".stripMargin
    ) { schema =>
      validateJson(schema)("""13""") { result =>
        assertEquals(
          result.results,
          Seq(
            ValidationResult(
              Seq(
                WithPointer(MinimumMismatch(13, true))
              )
            )
          )
        )
      }
      validateJson(schema)("""14""") { result =>
        assert(result.valid)
        assertEquals(result.annotations, Seq())
      }
    }
  }

  test("maxLength") {
    withSchema(
      """|{"maxLength": 3
         |}""".stripMargin
    ) { schema =>
      validateJson(schema)(""""toto"""") { result =>
        assertEquals(
          result.results,
          Seq(
            ValidationResult(
              Seq(
                WithPointer(MaxLengthMismatch(3))
              )
            )
          )
        )
      }
      validateJson(schema)(""""bar"""") { result =>
        assert(result.valid)
        assertEquals(result.annotations, Seq())
      }
    }
  }

  test("minLength") {
    withSchema(
      """|{"minLength": 4
         |}""".stripMargin
    ) { schema =>
      validateJson(schema)(""""bar"""") { result =>
        assertEquals(
          result.results,
          Seq(
            ValidationResult(
              Seq(
                WithPointer(MinLengthMismatch(4))
              )
            )
          )
        )
      }
      validateJson(schema)(""""toto"""") { result =>
        assert(result.valid)
        assertEquals(result.annotations, Seq())
      }
    }
  }

  test("pattern") {
    withSchema(
      """|{"pattern": "foo\\d\\d"
         |}""".stripMargin
    ) { schema =>
      validateJson(schema)(""""foo"""") { result =>
        assertEquals(
          result.results,
          Seq(
            ValidationResult(
              Seq(
                WithPointer(PatternMismatch("foo\\d\\d"))
              )
            )
          )
        )
      }
      validateJson(schema)(""""foo13"""") { result =>
        assert(result.valid)
        assertEquals(result.annotations, Seq())
      }
    }
  }

  test("minItems") {
    withSchema(
      """|{"minItems": 3
         |}""".stripMargin
    ) { schema =>
      validateJson(schema)("""[1,2]""") { result =>
        assertEquals(
          result.results,
          Seq(
            ValidationResult(
              Seq(
                WithPointer(MinItemsMismatch(3))
              )
            )
          )
        )
      }
      validateJson(schema)("""[2,3,4]""") { result =>
        assert(result.valid)
        assertEquals(result.annotations, Seq())
      }
    }
  }

  test("maxItems") {
    withSchema(
      """|{"maxItems": 2
         |}""".stripMargin
    ) { schema =>
      validateJson(schema)("""[1,2,3]""") { result =>
        assertEquals(
          result.results,
          Seq(
            ValidationResult(
              Seq(
                WithPointer(MaxItemsMismatch(2))
              )
            )
          )
        )
      }
      validateJson(schema)("""[2,3]""") { result =>
        assert(result.valid)
        assertEquals(result.annotations, Seq())
      }
    }
  }

  test("uniqueItems") {
    withSchema(
      """|{"uniqueItems": true
         |}""".stripMargin
    ) { schema =>
      validateJson(schema)("""[1,1]""") { result =>
        assertEquals(
          result.results,
          Seq(
            ValidationResult(
              Seq(
                WithPointer(ItemsNotUnique())
              )
            )
          )
        )
      }
      validateJson(schema)("""[13]""") { result =>
        assert(result.valid)
        assertEquals(result.annotations, Seq())
      }
    }
  }

  test("maxProperties") {
    withSchema(
      """|{"maxProperties": 2
         |}""".stripMargin
    ) { schema =>
      validateJson(schema)("""{"gnu": 1, "bar": 2, "foo": 3}""") { result =>
        assertEquals(
          result.results,
          Seq(
            ValidationResult(
              Seq(
                WithPointer(MaxPropertiesMismatch(2))
              )
            )
          )
        )
      }
      validateJson(schema)("""{"bar": 2, "foo": 3}""") { result =>
        assert(result.valid)
        assertEquals(result.annotations, Seq())
      }
    }
  }

  test("minProperties") {
    withSchema(
      """|{"minProperties": 3
         |}""".stripMargin
    ) { schema =>
      validateJson(schema)("""{"bar": 2, "foo": 3}""") { result =>
        assertEquals(
          result.results,
          Seq(
            ValidationResult(
              Seq(
                WithPointer(MinPropertiesMismatch(3))
              )
            )
          )
        )
      }
      validateJson(schema)("""{"gnu": 1, "bar": 2, "foo": 3}""") { result =>
        assert(result.valid)
        assertEquals(result.annotations, Seq())
      }
    }
  }

  test("required") {
    withSchema(
      """|{"required": ["bar", "foo"]
         |}""".stripMargin
    ) { schema =>
      validateJson(schema)("""{"gnu": 1, "bar": 2}""") { result =>
        assertEquals(
          result.results,
          Seq(
            ValidationResult(
              Seq(
                WithPointer(MissingRequiredProperties(Seq("foo")))
              )
            )
          )
        )
      }
      validateJson(schema)("""{"bar": 2, "foo": 3}""") { result =>
        assert(result.valid)
        assertEquals(result.annotations, Seq())
      }
    }
  }

  test("dependentRequired") {
    withSchema(
      """|{"dependentRequired": {"foo": ["bar", "gnu"]}
         |}""".stripMargin
    ) { schema =>
      validateJson(schema)("""{"foo": 1, "bar": 2}""") { result =>
        assertEquals(
          result.results,
          Seq(
            ValidationResult(
              Seq(
                WithPointer(DependentRequiredMissing(Map("foo" -> Seq("gnu"))))
              )
            )
          )
        )
      }
      validateJson(schema)("""{"foo": 1, "bar": 2, "gnu": 3}""") { result =>
        assert(result.valid)
        assertEquals(result.annotations, Seq())
      }
    }
  }

  test("dependentSchemas") {
    withSchema(
      """|{"dependentSchemas": {"foo": true, "gnu": false}}
         |}""".stripMargin
    ) { schema =>
      validateJson(schema)("""{"gnu": 1}""") { result =>
        assertEquals(
          result.results,
          Seq(
            ValidationResult(
              Seq(
                WithPointer(FalseSchemaReason())
              )
            )
          )
        )
      }
      validateJson(schema)("""{"foo": 1}""") { result =>
        assert(result.valid)
        assertEquals(result.annotations, Seq())
      }
    }
  }

  test("prefixItems") {
    withSchema(
      """|{"prefixItems": [{"type": "number"}, {"type": "string"}]
         |}""".stripMargin
    ) { schema =>
      validateJson(schema)("""["gnu"]""") { result =>
        assertEquals(
          result.results,
          Seq(
            ValidationResult(
              Seq(
                typedjson.processor.WithPointer(TypeMismatch("number"), Pointer.empty / 0)
              )
            )
          )
        )
      }
      validateJson(schema)("""[13, "foo", true]""") { result =>
        assert(result.valid)
        assertEquals(
          result.annotations,
          Seq(
            typedjson.processor.WithPointer(EvaluatedIndices(Seq(0, 1)), Pointer.empty)
          )
        )
      }
    }
  }

  test("prefixItems and items") {
    withSchema(
      """|{"prefixItems": [{"type": "number"}, {"type": "string"}],
         |"items": {"type": "boolean"}
         |}""".stripMargin
    ) { schema =>
      validateJson(schema)("""[13, "gnu", "boom"]""") { result =>
        assertEquals(
          result.results,
          Seq(
            ValidationResult(
              Seq(
                typedjson.processor.WithPointer(TypeMismatch("boolean"), Pointer.empty / 2)
              )
            )
          )
        )
      }
      validateJson(schema)("""[13, "foo", true]""") { result =>
        assert(result.valid)
        assertEquals(
          result.annotations,
          Seq(
            typedjson.processor.WithPointer(EvaluatedIndices(Seq(0, 1, 2)), Pointer.empty)
          )
        )
      }
    }
  }

  test("contains") {
    withSchema(
      """|{"contains": {"type": "number"}
         |}""".stripMargin
    ) { schema =>
      validateJson(schema)("""["gnu", true]""") { result =>
        assertEquals(
          result.results,
          Seq(
            ValidationResult(
              Seq(
                WithPointer(NotContains(0))
              )
            )
          )
        )
      }
      validateJson(schema)("""[13, "foo", true]""") { result =>
        assert(result.valid)
        assertEquals(result.annotations, Seq(typedjson.processor.WithPointer(EvaluatedIndices(Seq(0)), Pointer.empty)))
      }
      validateJson(schema)("""[13, 14, "foo", true]""") { result =>
        assert(result.valid)
        assertEquals(
          result.annotations,
          Seq(
            typedjson.processor.WithPointer(EvaluatedIndices(Seq(0, 1)), Pointer.empty)
          )
        )
      }
    }
  }

  test("minContains") {
    withSchema(
      """|{"contains": {"type": "number"},
         |"minContains": 2
         |}""".stripMargin
    ) { schema =>
      validateJson(schema)("""[13, "gnu", true]""") { result =>
        assertEquals(
          result.results,
          Seq(
            ValidationResult(
              Seq(
                WithPointer(NotContains(1))
              )
            )
          )
        )
      }
      validateJson(schema)("""[13, 14, "foo", true]""") { result =>
        assert(result.valid)
        assertEquals(
          result.annotations,
          Seq(typedjson.processor.WithPointer(EvaluatedIndices(Seq(0, 1)), Pointer.empty))
        )
      }
    }
  }

  test("maxContains") {
    withSchema(
      """|{"contains": {"type": "number"},
         |"maxContains": 2
         |}""".stripMargin
    ) { schema =>
      validateJson(schema)("""[13, 14, 15, "gnu", true]""") { result =>
        assertEquals(
          result.results,
          Seq(
            ValidationResult(
              Seq(
                WithPointer(NotContains(3))
              )
            )
          )
        )
      }
      validateJson(schema)("""[]""") { result =>
        assertEquals(
          result.results,
          Seq(
            ValidationResult(
              Seq(
                WithPointer(NotContains(0))
              )
            )
          )
        )
      }
      validateJson(schema)("""[13, 14, "foo", true]""") { result =>
        assert(result.valid)
        assertEquals(
          result.annotations,
          Seq(typedjson.processor.WithPointer(EvaluatedIndices(Seq(0, 1)), Pointer.empty))
        )
      }
    }
  }

  test("minContains without contains") {
    withSchema(
      """|{"minContains": 2
         |}""".stripMargin
    ) { schema =>
      validateJson(schema)("""[13, "gnu", true]""") { result =>
        assert(result.valid)
        assertEquals(result.annotations, Seq())
      }
      validateJson(schema)("""[13, 14, "foo", true]""") { result =>
        assert(result.valid)
        assertEquals(result.annotations, Seq())
      }
    }
  }

  test("maxContains without contains") {
    withSchema(
      """|{"maxContains": 2
         |}""".stripMargin
    ) { schema =>
      validateJson(schema)("""[13, "gnu", true]""") { result =>
        assert(result.valid)
        assertEquals(result.annotations, Seq())
      }
      validateJson(schema)("""[13, 14, "foo", true]""") { result =>
        assert(result.valid)
        assertEquals(result.annotations, Seq())
      }
    }
  }

  test("patternProperties") {
    withSchema(
      """|{"patternProperties": { "^f": {} },
         |"additionalProperties": false
         |}""".stripMargin
    ) { schema =>
      validateJson(schema)("""{"gnu": 13, "bar": true}""") { result =>
        assertEquals(
          result.results,
          Seq(
            ValidationResult(
              Seq(
                typedjson.processor.WithPointer(FalseSchemaReason(), Pointer.empty / "gnu"),
                typedjson.processor.WithPointer(FalseSchemaReason(), Pointer.empty / "bar")
              )
            )
          )
        )
      }
      validateJson(schema)("""{"foo": "ok"}]""") { result =>
        assertEquals(result.results, Seq())
        assert(result.valid)
        assertEquals(
          result.annotations,
          Seq(typedjson.processor.WithPointer(EvaluatedProperties(Set("foo")), Pointer.empty))
        )
      }
    }
  }

  test("missing deep lazy $ref raises error") {
    withSchema(
      """|{
         |"$id": "http://myhost:1313/",
         |"items": {"$ref": "myItems"},
         |"$defs": {
         |  "foo": {
         |    "$id": "myItems",
         |    "items": {"$ref": "missing.json"}
         |  }
         |}
         |}
         |""".stripMargin
    ) { schema =>
      validateJson(schema)("""[ 13 ]""") { result =>
        assertEquals(result.valid, false)
        assertEquals(
          result.problems.errors,
          Seq(WithPointer(MissingReference("missing.json"), Pointer.parse("/items/$ref")))
        )
        assertEquals(result.annotations, Seq())
      }
    }
  }
}
