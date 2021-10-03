package frawa.typedjson.schema

import munit.FunSuite
import frawa.typedjson.parser.Value
import frawa.typedjson.parser.ZioParser
import frawa.typedjson.parser.Parser
import frawa.typedjson.parser.NumberValue
import frawa.typedjson.parser.BoolValue
import frawa.typedjson.parser.NullValue
import frawa.typedjson.parser.StringValue
import frawa.typedjson.parser.ArrayValue
import frawa.typedjson.parser.ObjectValue
import scala.reflect.internal.Reporter
import munit.Assertions._
import TestUtil._
import TestSchemas._

class ChecksTest extends FunSuite {
  implicit val zioParser = new ZioParser()

  private def assertChecks(schema: SchemaValue, allowIgnored: Boolean = false)(
      f: Checks => Unit
  ) = {
    implicit val resolver = LoadedSchemasResolver(schema)
    val withParsed = for {
      checks <- Checks.parseKeywords(schema)
    } yield {
      if (!allowIgnored) {
        assert(
          checks.ignoredKeywords.isEmpty,
          clue(s"""unexpected ignored keywords: ${checks.ignoredKeywords.mkString(",")}""")
        )
      }
      f(checks)
    }
    withParsed.swap
      .map(messages => fail("parsing keywords failed", clues(clue(messages))))
      .swap
  }

  private def assertChecksWithIgnored(schema: SchemaValue) = assertChecks(schema, true) _

  private def assertSchemaErrors(schema: SchemaValue)(
      f: Checks.SchemaErrors => Unit
  ) = {
    implicit val resolver = LoadedSchemasResolver(schema)
    Checks.parseKeywords(schema) match {
      case Right(_)     => fail("parsing keywords expected to fail")
      case Left(errors) => f(errors)
    }
  }

  test("null") {
    withSchema(nullSchema) { schema =>
      assertChecks(schema) { checks =>
        assertEquals(checks.checks, Seq(NullTypeCheck))
      }
    }
  }

  test("boolean") {
    withSchema(boolSchema) { schema =>
      assertChecks(schema) { checks =>
        assertEquals(checks.checks, Seq(BooleanTypeCheck))
      }
    }
  }

  test("true schema") {
    withSchema(trueSchema) { schema =>
      assertChecks(schema) { checks =>
        assertEquals(checks.checks, Seq(TrivialCheck(true)))
      }
    }
  }

  test("false schema") {
    withSchema(falseSchema) { schema =>
      assertChecks(schema) { checks =>
        assertEquals(checks.checks, Seq(TrivialCheck(false)))
      }
    }
  }

  test("ignored keyword") {
    withSchema("""{"ignored": false}""") { schema =>
      assertChecksWithIgnored(schema) { checks =>
        assertEquals(checks.ignoredKeywords, Set("ignored"))
      }
    }
  }

  test("not false") {
    withSchema(notFalseSchema) { schema =>
      assertChecks(schema) { checks =>
        assertEquals(
          checks.checks,
          Seq(
            NotCheck(
              Checks(
                SchemaValue(
                  value = BoolValue(
                    value = false
                  )
                ),
                Seq(TrivialCheck(false))
              )
            )
          )
        )
      }
    }
  }

  test("invalid schema") {
    withSchema("""13""") { schema =>
      assertSchemaErrors(schema) { errors =>
        assertEquals(errors, Seq(SchemaError("invalid schema SchemaValue(NumberValue(13))")))
      }
    }
  }

  test("invalid deep schema") {
    withSchema("""{"not": "gnu"}""") { schema =>
      assertSchemaErrors(schema) { errors =>
        assertEquals(errors, Seq(SchemaError("invalid schema SchemaValue(StringValue(gnu))", Pointer.empty / "not")))
      }
    }
  }

  test("empty schema") {
    withSchema(emtpySchema) { schema =>
      assertChecks(schema) { checks =>
        assertEquals(checks.checks, Seq(TrivialCheck(true)))
      }
    }
  }

  test("string") {
    withSchema(stringSchema) { schema =>
      assertChecks(schema) { checks =>
        assertEquals(checks.checks, Seq(StringTypeCheck))
      }
    }
  }

  test("number") {
    withSchema(numberSchema) { schema =>
      assertChecks(schema) { checks =>
        assertEquals(checks.checks, Seq(NumberTypeCheck))
      }
    }
  }

  test("array") {
    withSchema(arraySchema) { schema =>
      assertChecks(schema) { checks =>
        assertEquals(checks.checks, Seq(ArrayTypeCheck))
      }
    }
  }

  test("array with items") {
    withSchema(numberArraySchema) { schema =>
      assertChecks(schema) { checks =>
        assertEquals(
          checks.checks,
          Seq(
            ArrayTypeCheck,
            ArrayItemsCheck(
              Some(
                Checks(
                  numberSchemaValue,
                  Seq(NumberTypeCheck)
                )
              )
            )
          )
        )
      }
    }
    withSchema("""{"items": { "type": "number"}, "type": "array"}""") { schema =>
      assertChecks(schema) { checks =>
        assertEquals(
          checks.checks,
          Seq(
            ArrayItemsCheck(
              Some(
                Checks(
                  numberSchemaValue,
                  Seq(NumberTypeCheck)
                )
              )
            ),
            ArrayTypeCheck
          )
        )
      }
    }
  }

  test("object") {
    withSchema(totoObjectSchema) { schema =>
      assertChecks(schema) { checks =>
        assertEquals(
          checks.checks,
          Seq(
            ObjectTypeCheck,
            ObjectPropertiesCheck(
              Map(
                "toto" -> Checks(
                  numberSchemaValue,
                  checks = List(
                    NumberTypeCheck
                  ),
                  ignoredKeywords = Set()
                ),
                "titi" -> Checks(
                  stringSchemaValue,
                  checks = List(
                    StringTypeCheck
                  ),
                  ignoredKeywords = Set()
                )
              )
            )
          )
        )
      }
    }
  }

  test("required") {
    withSchema("""{
                 |"type": "object",
                 |"required": ["titi"]
                 |}
                 |""".stripMargin) { schema =>
      assertChecks(schema) { checks =>
        assertEquals(
          checks.checks,
          Seq(
            ObjectTypeCheck,
            ObjectRequiredCheck(
              names = IndexedSeq(
                "titi"
              )
            )
          )
        )
      }
    }
  }

  test("allOf") {
    withSchema(allOfSchema) { schema =>
      assertChecks(schema) { checks =>
        assertEquals(
          checks.checks,
          Seq(
            AllOfCheck(
              Seq(
                Checks(
                  numberSchemaValue,
                  checks = List(
                    NumberTypeCheck
                  ),
                  ignoredKeywords = Set()
                )
              )
            )
          )
        )
      }
    }
  }

  test("anyOf") {
    withSchema(anyOfSchema) { schema =>
      assertChecks(schema) { checks =>
        assertEquals(
          checks.checks,
          Seq(
            AnyOfCheck(
              Seq(
                Checks(
                  schema = SchemaValue(
                    value = ObjectValue(
                      properties = Map(
                        "type" -> StringValue(
                          value = "number"
                        )
                      )
                    )
                  ),
                  checks = List(
                    NumberTypeCheck
                  ),
                  ignoredKeywords = Set()
                ),
                Checks(
                  schema = SchemaValue(
                    value = ObjectValue(
                      properties = Map(
                        "type" -> StringValue(
                          value = "string"
                        )
                      )
                    )
                  ),
                  checks = List(
                    StringTypeCheck
                  ),
                  ignoredKeywords = Set()
                )
              )
            )
          )
        )
      }
    }
  }

  test("oneOf") {
    withSchema(oneOfSchema) { schema =>
      assertChecks(schema) { checks =>
        assertEquals(
          checks.checks,
          Seq(
            OneOfCheck(
              Seq(
                Checks(
                  numberSchemaValue,
                  checks = List(
                    NumberTypeCheck
                  ),
                  ignoredKeywords = Set()
                ),
                Checks(
                  stringSchemaValue,
                  checks = List(
                    StringTypeCheck
                  ),
                  ignoredKeywords = Set()
                )
              )
            )
          )
        )
      }
    }
  }

  test("if/then/else") {
    withSchema(ifThenElseSchema) { schema =>
      assertChecks(schema) { checks =>
        assertEquals(
          checks.checks,
          Seq(
            IfThenElseCheck(
              Some(Checks(numberSchemaValue, Seq(NumberTypeCheck), Set())),
              Some(Checks(numberSchemaValue, Seq(NumberTypeCheck), Set())),
              Some(Checks(stringSchemaValue, Seq(StringTypeCheck), Set()))
            )
          )
        )
      }
    }
  }

  test("null or string") {
    withSchema(nullOrStringSchema) { schema =>
      assertChecks(schema) { checks =>
        assertEquals(
          checks.checks,
          Seq(
            UnionTypeCheck(
              Seq(
                NullTypeCheck,
                StringTypeCheck
              )
            )
          )
        )
      }
    }
  }

  test("enum") {
    withSchema(enumSchema) { schema =>
      assertChecks(schema) { checks =>
        assertEquals(
          checks.checks,
          Seq(
            StringTypeCheck,
            EnumCheck(
              values = Seq(
                StringValue(
                  value = "foo"
                ),
                StringValue(
                  value = "bar"
                )
              )
            )
          )
        )
      }
    }
  }

  test("const") {
    withSchema(constSchema) { schema =>
      assertChecks(schema) { checks =>
        assertEquals(
          checks.checks,
          Seq(
            StringTypeCheck,
            EnumCheck(
              values = Seq(
                StringValue(
                  value = "first"
                )
              )
            )
          )
        )
      }
    }
  }

  test("$id/$ref/$def") {
    withSchema(idRefDefsSchema) { schema =>
      assertChecks(schema) { checks =>
        assertEquals(
          checks.checks,
          Seq(
            ArrayTypeCheck,
            ArrayItemsCheck(
              items = Some(
                value = Checks(
                  schema = SchemaValue(
                    value = ObjectValue(
                      properties = Map(
                        "$anchor" -> StringValue(
                          value = "item"
                        ),
                        "type" -> StringValue(
                          value = "number"
                        )
                      )
                    )
                  ),
                  checks = List(
                    NumberTypeCheck
                  ),
                  ignoredKeywords = Set()
                )
              )
            )
          )
        )
      }
    }
  }

  test("recursive $ref/$def".only) {
    // TODO assert more precisely
    withSchema(recursiveRefDefsSchema) { schema =>
      assertChecks(schema) { checks =>
        assertEquals(
          checks.checks.size,
          1
        )
      // assertEquals(
      //   checks.checks,
      //   Seq()
      // )
      }
    }
  }

  test("subitem $ref/$def") {
    withSchema(subItemRefDefsSchema) { schema =>
      assertChecks(schema) { checks =>
        assertEquals(
          checks.checks.length,
          2
        )
      }
    }
  }
}
