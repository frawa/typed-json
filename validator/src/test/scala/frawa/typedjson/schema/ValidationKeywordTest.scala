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

package frawa.typedjson.schemaSpec

import frawa.typedjson.parser.ZioParser
import munit.FunSuite
import frawa.typedjson.schema.SchemaValue
import frawa.typedjson.schema.Checked
import frawa.typedjson.schema.ValidationResult
import frawa.typedjson.schema.TestUtil._
import frawa.typedjson.schema._

class ValidationKeywordTest extends FunSuite {
  implicit val zioParser: ZioParser = new ZioParser()

  def validateJson(schema: SchemaValue)(jsonText: String)(f: Checked[ValidationResult] => Unit): Either[Nothing,Unit] = {
    assertChecked(ValidationChecker())(schema, jsonText)(f)
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
        assertEquals(checked.annotations, Seq())
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
        assertEquals(checked.annotations, Seq())
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
        assertEquals(checked.annotations, Seq())
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
        assertEquals(checked.annotations, Seq())
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
        assertEquals(checked.annotations, Seq())
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
        assertEquals(checked.annotations, Seq())
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
        assertEquals(checked.annotations, Seq())
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
        assertEquals(checked.annotations, Seq())
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
        assertEquals(checked.annotations, Seq())
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
        assertEquals(checked.annotations, Seq())
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
        assertEquals(checked.annotations, Seq())
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
        assertEquals(checked.annotations, Seq())
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
        assertEquals(checked.annotations, Seq())
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
        assertEquals(checked.annotations, Seq())
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
        assertEquals(checked.annotations, Seq())
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
        assertEquals(checked.annotations, Seq())
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
        assertEquals(
          checked.annotations,
          Seq(
            WithPointer(EvaluatedIndices(Seq(0, 1)), Pointer.empty)
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
        assertEquals(
          checked.annotations,
          Seq(
            WithPointer(EvaluatedIndices(Seq(0, 1, 2)), Pointer.empty)
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
        assertEquals(checked.annotations, Seq(WithPointer(EvaluatedIndices(Seq(0)), Pointer.empty)))
      }
      validateJson(schema)("""[13, 14, "foo", true]""") { checked =>
        assert(checked.valid)
        assertEquals(
          checked.annotations,
          Seq(
            WithPointer(EvaluatedIndices(Seq(0, 1)), Pointer.empty)
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
        assertEquals(checked.annotations, Seq(WithPointer(EvaluatedIndices(Seq(0, 1)), Pointer.empty)))
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
        assertEquals(checked.annotations, Seq(WithPointer(EvaluatedIndices(Seq(0, 1)), Pointer.empty)))
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
        assertEquals(checked.annotations, Seq())
      }
      validateJson(schema)("""[13, 14, "foo", true]""") { checked =>
        assert(checked.valid)
        assertEquals(checked.annotations, Seq())
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
        assertEquals(checked.annotations, Seq())
      }
      validateJson(schema)("""[13, 14, "foo", true]""") { checked =>
        assert(checked.valid)
        assertEquals(checked.annotations, Seq())
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
        assertEquals(checked.annotations, Seq(WithPointer(EvaluatedProperties(Set("foo")), Pointer.empty)))
      }
    }
  }
}
