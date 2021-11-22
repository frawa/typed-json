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

package frawa.typedjson.processor

import munit.FunSuite
import frawa.typedjson.parser.{BoolValue, NumberValue, ObjectValue, StringValue, ZioParser}
import frawa.typedjson.processor.SchemaProblems.{InvalidSchemaValue, SchemaError}
import frawa.typedjson.testutil.TestUtil._
import frawa.typedjson.testutil.TestSchemas._

class ChecksTest extends FunSuite {
  import frawa.typedjson.util.UriUtil._

  implicit val zioParser: ZioParser = new ZioParser()

  private def assertChecks(schema: SchemaValue, allowIgnored: Boolean = false)(
      f: Checks => Unit
  ) = {
    implicit val resolver = LoadedSchemasResolver(schema)
    val scope             = DynamicScope.empty
    val withParsed = for {
      checks <- Checks.parseKeywords(schema, scope)
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
    val scope             = DynamicScope.empty
    Checks.parseKeywords(schema, scope) match {
      case Right(_)     => fail("parsing keywords expected to fail")
      case Left(errors) => f(errors)
    }
  }

  private val noResolve: () => Either[Seq[SchemaError], Checks] = () => Left(Seq.empty[SchemaError])

  // TODO use implicits?
  private def assertable(checks: Checks): Checks = checks.copy(checks = checks.checks.map(assertable))
  // private def assertable[T](checks: Seq[T]): Seq[T] = checks.map(assertable)
  private def assertable(check: Checks.CheckWithLocation): Checks.CheckWithLocation =
    check.copy(value = assertable(check.value))
  private def assertable(check: Check): Check = check match {
    case ArrayItemsCheck(items, prefixItems) =>
      ArrayItemsCheck(
        items.map(assertable),
        prefixItems.map(assertable)
      )
    case LazyResolveCheck(resolved, resolve) => LazyResolveCheck(resolved, noResolve)
    case _                                   => check
  }

  test("null") {
    withSchema(nullSchema) { schema =>
      assertChecks(schema) { checks =>
        assertEquals(checks.checks, Seq(WithLocation(uri("#/type"), NullTypeCheck)))
      }
    }
  }

  test("boolean") {
    withSchema(boolSchema) { schema =>
      assertChecks(schema) { checks =>
        assertEquals(checks.checks, Seq(WithLocation(uri("#/type"), BooleanTypeCheck)))
      }
    }
  }

  test("true schema") {
    withSchema(trueSchema) { schema =>
      assertChecks(schema) { checks =>
        assertEquals(checks.checks, Seq(WithLocation(uri("#"), TrivialCheck(true))))
      }
    }
  }

  test("false schema") {
    withSchema(falseSchema) { schema =>
      assertChecks(schema) { checks =>
        assertEquals(checks.checks, Seq(WithLocation(uri("#"), TrivialCheck(false))))
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
            WithLocation(
              uri("#/not"),
              NotCheck(
                Checks(
                  SchemaValue(
                    value = BoolValue(
                      value = false
                    )
                  ),
                  Seq(WithLocation(uri("#/not"), TrivialCheck(false)))
                )
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
        assertEquals(errors, Seq(WithPointer(InvalidSchemaValue(NumberValue(13)))))
      }
    }
  }

  test("invalid deep schema") {
    withSchema("""{"not": "gnu"}""") { schema =>
      assertSchemaErrors(schema) { errors =>
        assertEquals(errors, Seq(WithPointer(InvalidSchemaValue(StringValue("gnu")), Pointer.empty / "not")))
      }
    }
  }

  test("empty schema") {
    withSchema(emtpySchema) { schema =>
      assertChecks(schema) { checks =>
        assertEquals(checks.checks, Seq(WithLocation(uri("#"), TrivialCheck(true))))
      }
    }
  }

  test("string") {
    withSchema(stringSchema) { schema =>
      assertChecks(schema) { checks =>
        assertEquals(checks.checks, Seq(WithLocation(uri("#/type"), StringTypeCheck)))
      }
    }
  }

  test("number") {
    withSchema(numberSchema) { schema =>
      assertChecks(schema) { checks =>
        assertEquals(checks.checks, Seq(WithLocation(uri("#/type"), NumberTypeCheck)))
      }
    }
  }

  test("array") {
    withSchema(arraySchema) { schema =>
      assertChecks(schema) { checks =>
        assertEquals(checks.checks, Seq(WithLocation(uri("#/type"), ArrayTypeCheck)))
      }
    }
  }

  test("array with items") {
    withSchema(numberArraySchema) { schema =>
      assertChecks(schema) { checks =>
        assertEquals(
          checks.checks,
          Seq(
            WithLocation(uri("#/type"), ArrayTypeCheck),
            WithLocation(
              uri("#/items"),
              ArrayItemsCheck(
                Some(
                  Checks(
                    numberSchemaValue,
                    Seq(WithLocation(uri("#/items/type"), NumberTypeCheck))
                  )
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
            WithLocation(
              uri("#/items"),
              ArrayItemsCheck(
                Some(
                  Checks(
                    numberSchemaValue,
                    Seq(WithLocation(uri("#/items/type"), NumberTypeCheck))
                  )
                )
              )
            ),
            WithLocation(uri("#/type"), ArrayTypeCheck)
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
            WithLocation(uri("#/type"), ObjectTypeCheck),
            WithLocation(
              uri("#/properties"),
              ObjectPropertiesCheck(
                Map(
                  "toto" -> Checks(
                    numberSchemaValue,
                    checks = List(
                      WithLocation(uri("#/properties/toto/type"), NumberTypeCheck)
                    ),
                    ignoredKeywords = Set()
                  ),
                  "titi" -> Checks(
                    stringSchemaValue,
                    checks = List(
                      WithLocation(uri("#/properties/titi/type"), StringTypeCheck)
                    ),
                    ignoredKeywords = Set()
                  )
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
            WithLocation(uri("#/type"), ObjectTypeCheck),
            WithLocation(
              uri("#/required"),
              ObjectRequiredCheck(
                names = IndexedSeq(
                  "titi"
                )
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
            WithLocation(
              uri("#/allOf"),
              AllOfCheck(
                Seq(
                  Checks(
                    numberSchemaValue,
                    checks = List(
                      WithLocation(uri("#/allOf/0/type"), NumberTypeCheck)
                    ),
                    ignoredKeywords = Set()
                  )
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
            WithLocation(
              uri("#/anyOf"),
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
                      WithLocation(uri("#/anyOf/0/type"), NumberTypeCheck)
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
                      WithLocation(uri("#/anyOf/1/type"), StringTypeCheck)
                    ),
                    ignoredKeywords = Set()
                  )
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
            WithLocation(
              uri("#/oneOf"),
              OneOfCheck(
                Seq(
                  Checks(
                    numberSchemaValue,
                    checks = List(
                      WithLocation(uri("#/oneOf/0/type"), NumberTypeCheck)
                    ),
                    ignoredKeywords = Set()
                  ),
                  Checks(
                    stringSchemaValue,
                    checks = List(
                      WithLocation(uri("#/oneOf/1/type"), StringTypeCheck)
                    ),
                    ignoredKeywords = Set()
                  )
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
            WithLocation(
              uri("#/if"),
              IfThenElseCheck(
                Some(Checks(numberSchemaValue, Seq(WithLocation(uri("#/if/type"), NumberTypeCheck)), Set())),
                Some(Checks(numberSchemaValue, Seq(WithLocation(uri("#/then/type"), NumberTypeCheck)), Set())),
                Some(Checks(stringSchemaValue, Seq(WithLocation(uri("#/else/type"), StringTypeCheck)), Set()))
              )
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
            WithLocation(
              uri("#/type"),
              UnionTypeCheck(
                Seq(
                  WithLocation(uri("#/type"), NullTypeCheck),
                  WithLocation(uri("#/type"), StringTypeCheck)
                )
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
            WithLocation(uri("#/type"), StringTypeCheck),
            WithLocation(
              uri("#/enum"),
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
            WithLocation(uri("#/type"), StringTypeCheck),
            WithLocation(
              uri("#/const"),
              EnumCheck(
                values = Seq(
                  StringValue(
                    value = "first"
                  )
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
          assertable(checks).checks,
          Seq(
            WithLocation(uri("https://example.net/root.json#/type"), ArrayTypeCheck),
            WithLocation(
              uri("https://example.net/root.json#/items"),
              ArrayItemsCheck(
                items = Some(
                  value = Checks(
                    schema = SchemaValue(
                      value = ObjectValue(
                        properties = Map(
                          "$ref" -> StringValue(
                            value = "#item"
                          )
                        )
                      )
                    ),
                    checks = List(
                      WithLocation(
                        uri("https://example.net/root.json#/items/$ref"),
                        new LazyResolveCheck(uri("https://example.net/root.json#item"), noResolve)
                      )
                    ),
                    ignoredKeywords = Set()
                  )
                )
              )
            )
          )
        )
      }
    }
  }

  test("recursive $ref/$def") {
    // TODO assert more precisely
    withSchema(recursiveRefDefsSchema) { schema =>
      assertChecks(schema) { checks =>
        assertEquals(
          checks.checks.size,
          1
        )
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
