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

package frawa.typedjson.keywords

import frawa.typedjson.keywords.SchemaProblems.InvalidSchemaValue
import frawa.typedjson.parser.Value._
import frawa.typedjson.pointer.Pointer
import frawa.typedjson.testutil.TestSchemas._
import frawa.typedjson.testutil.TestUtil._
import frawa.typedjson.util.UriUtil.{WithLocation, uri}
import munit.FunSuite

class KeywordsTest extends FunSuite {
  private val vocabularyForTest = dialect(Seq(Vocabulary.coreId, Vocabulary.validationId, Vocabulary.applicatorId)).get

  private def assertKeywords(schema: SchemaValue, allowIgnored: Boolean = false)(
      f: Keywords => Unit
  ): Either[Nothing, Unit] = {
    val resolver: LoadedSchemasResolver = LoadedSchemasResolver(schema)
    val scope                           = DynamicScope.empty
    val withParsed = for {
      keywords <- Keywords.parseKeywords(vocabularyForTest, resolver.push(schema), scope)
    } yield {
      if (!allowIgnored) {
        assertEquals(
          keywords.ignored,
          Set.empty[String],
          clue("unexpected ignored keywords")
        )
      }
      f(keywords)
    }
    withParsed.swap
      .map(messages => fail("parsing keywords failed", clues(clue(messages))))
      .swap
  }

  private def assertKeywordsWithIgnored(schema: SchemaValue) = assertKeywords(schema, allowIgnored = true) _

  private def assertSchemaProblems(schema: SchemaValue)(
      f: SchemaProblems => Unit
  ): Unit = {
    val resolver: LoadedSchemasResolver = LoadedSchemasResolver(schema)
    val scope                           = DynamicScope.empty
    Keywords.parseKeywords(vocabularyForTest, resolver.push(schema), scope) match {
      case Right(_)     => fail("parsing keywords expected to fail")
      case Left(errors) => f(errors)
    }
  }

  test("null") {
    withSchema(nullSchema) { schema =>
      assertKeywords(schema) { keywords =>
        assertEquals(keywords.keywords, Seq(WithLocation(uri("#/type"), NullTypeKeyword)))
      }
    }
  }

  test("boolean") {
    withSchema(boolSchema) { schema =>
      assertKeywords(schema) { keywords =>
        assertEquals(keywords.keywords, Seq(WithLocation(uri("#/type"), BooleanTypeKeyword)))
      }
    }
  }

  test("true schema") {
    withSchema(trueSchema) { schema =>
      assertKeywords(schema) { keywords =>
        assertEquals(keywords.keywords, Seq(WithLocation(uri("#"), TrivialKeyword(true))))
      }
    }
  }

  test("false schema") {
    withSchema(falseSchema) { schema =>
      assertKeywords(schema) { keywords =>
        assertEquals(keywords.keywords, Seq(WithLocation(uri("#"), TrivialKeyword(false))))
      }
    }
  }

  test("ignored keyword") {
    withSchema("""{"ignored": false}""") { schema =>
      assertKeywordsWithIgnored(schema) { keywords =>
        assertEquals(keywords.ignored, Set("ignored"))
      }
    }
  }

  test("not false") {
    withSchema(notFalseSchema) { schema =>
      assertKeywords(schema) { keywords =>
        assertEquals(
          keywords.keywords,
          Seq(
            WithLocation(
              uri("#/not"),
              NotKeyword(
                Keywords(
                  vocabularyForTest,
                  SchemaValue(
                    value = BoolValue(
                      value = false
                    )
                  ),
                  Seq(WithLocation(uri("#/not"), TrivialKeyword(false)))
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
      assertSchemaProblems(schema) { problems =>
        assertEquals(problems, SchemaProblems(InvalidSchemaValue(NumberValue(13))))
      }
    }
  }

  test("invalid deep schema") {
    withSchema("""{"not": "gnu"}""") { schema =>
      assertSchemaProblems(schema) { problems =>
        assertEquals(
          problems,
          SchemaProblems(Seq(WithPointer(InvalidSchemaValue(StringValue("gnu")), Pointer.empty / "not")))
        )
      }
    }
  }

  test("empty schema") {
    withSchema(emtpySchema) { schema =>
      assertKeywords(schema) { keywords =>
        assertEquals(keywords.keywords, Seq(WithLocation(uri("#"), TrivialKeyword(true))))
      }
    }
  }

  test("string") {
    withSchema(stringSchema) { schema =>
      assertKeywords(schema) { keywords =>
        assertEquals(keywords.keywords, Seq(WithLocation(uri("#/type"), StringTypeKeyword)))
      }
    }
  }

  test("number") {
    withSchema(numberSchema) { schema =>
      assertKeywords(schema) { keywords =>
        assertEquals(keywords.keywords, Seq(WithLocation(uri("#/type"), NumberTypeKeyword)))
      }
    }
  }

  test("array") {
    withSchema(arraySchema) { schema =>
      assertKeywords(schema) { keywords =>
        assertEquals(keywords.keywords, Seq(WithLocation(uri("#/type"), ArrayTypeKeyword)))
      }
    }
  }

  test("array with items") {
    withSchema(numberArraySchema) { schema =>
      assertKeywords(schema) { keywords =>
        assertEquals(
          keywords.keywords,
          Seq(
            WithLocation(uri("#/type"), ArrayTypeKeyword),
            WithLocation(
              uri("#/items"),
              ArrayItemsKeyword(
                Some(
                  Keywords(
                    vocabularyForTest,
                    numberSchemaValue,
                    Seq(WithLocation(uri("#/items/type"), NumberTypeKeyword))
                  )
                )
              )
            )
          )
        )
      }
    }
    withSchema("""{"items": { "type": "number"}, "type": "array"}""") { schema =>
      assertKeywords(schema) { keywords =>
        assertEquals(
          keywords.keywords,
          Seq(
            WithLocation(
              uri("#/items"),
              ArrayItemsKeyword(
                Some(
                  Keywords(
                    vocabularyForTest,
                    numberSchemaValue,
                    Seq(WithLocation(uri("#/items/type"), NumberTypeKeyword))
                  )
                )
              )
            ),
            WithLocation(uri("#/type"), ArrayTypeKeyword)
          )
        )
      }
    }
  }

  test("object") {
    withSchema(totoObjectSchema) { schema =>
      assertKeywords(schema) { keywords =>
        assertEquals(
          keywords.keywords,
          Seq(
            WithLocation(uri("#/type"), ObjectTypeKeyword),
            WithLocation(
              uri("#/properties"),
              ObjectPropertiesKeyword(
                Map(
                  "toto" -> Keywords(
                    vocabularyForTest,
                    numberSchemaValue,
                    keywords = List(
                      WithLocation(uri("#/properties/toto/type"), NumberTypeKeyword)
                    ),
                    ignored = Set()
                  ),
                  "titi" -> Keywords(
                    vocabularyForTest,
                    stringSchemaValue,
                    keywords = List(
                      WithLocation(uri("#/properties/titi/type"), StringTypeKeyword)
                    ),
                    ignored = Set()
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
      assertKeywords(schema) { keywords =>
        assertEquals(
          keywords.keywords,
          Seq(
            WithLocation(uri("#/type"), ObjectTypeKeyword),
            WithLocation(
              uri("#/required"),
              ObjectRequiredKeyword(
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
      assertKeywords(schema) { keywords =>
        assertEquals(
          keywords.keywords,
          Seq(
            WithLocation(
              uri("#/allOf"),
              AllOfKeyword(
                Seq(
                  Keywords(
                    vocabularyForTest,
                    numberSchemaValue,
                    keywords = List(
                      WithLocation(uri("#/allOf/0/type"), NumberTypeKeyword)
                    ),
                    ignored = Set()
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
      assertKeywords(schema) { keywords =>
        assertEquals(
          keywords.keywords,
          Seq(
            WithLocation(
              uri("#/anyOf"),
              AnyOfKeyword(
                Seq(
                  Keywords(
                    vocabularyForTest,
                    schema = SchemaValue(
                      value = ObjectValue(
                        properties = Map(
                          "type" -> StringValue(
                            value = "number"
                          )
                        )
                      )
                    ),
                    keywords = List(
                      WithLocation(uri("#/anyOf/0/type"), NumberTypeKeyword)
                    ),
                    ignored = Set()
                  ),
                  Keywords(
                    vocabularyForTest,
                    schema = SchemaValue(
                      value = ObjectValue(
                        properties = Map(
                          "type" -> StringValue(
                            value = "string"
                          )
                        )
                      )
                    ),
                    keywords = List(
                      WithLocation(uri("#/anyOf/1/type"), StringTypeKeyword)
                    ),
                    ignored = Set()
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
      assertKeywords(schema) { keywords =>
        assertEquals(
          keywords.keywords,
          Seq(
            WithLocation(
              uri("#/oneOf"),
              OneOfKeyword(
                Seq(
                  Keywords(
                    vocabularyForTest,
                    numberSchemaValue,
                    keywords = List(
                      WithLocation(uri("#/oneOf/0/type"), NumberTypeKeyword)
                    ),
                    ignored = Set()
                  ),
                  Keywords(
                    vocabularyForTest,
                    stringSchemaValue,
                    keywords = List(
                      WithLocation(uri("#/oneOf/1/type"), StringTypeKeyword)
                    ),
                    ignored = Set()
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
      assertKeywords(schema) { keywords =>
        assertEquals(
          keywords.keywords,
          Seq(
            WithLocation(
              uri("#/if"),
              IfThenElseKeyword(
                Some(
                  Keywords(
                    vocabularyForTest,
                    numberSchemaValue,
                    Seq(WithLocation(uri("#/if/type"), NumberTypeKeyword)),
                    Set()
                  )
                ),
                Some(
                  Keywords(
                    vocabularyForTest,
                    numberSchemaValue,
                    Seq(WithLocation(uri("#/then/type"), NumberTypeKeyword)),
                    Set()
                  )
                ),
                Some(
                  Keywords(
                    vocabularyForTest,
                    stringSchemaValue,
                    Seq(WithLocation(uri("#/else/type"), StringTypeKeyword)),
                    Set()
                  )
                )
              )
            )
          )
        )
      }
    }
  }

  test("null or string") {
    withSchema(nullOrStringSchema) { schema =>
      assertKeywords(schema) { keywords =>
        assertEquals(
          keywords.keywords,
          Seq(
            WithLocation(
              uri("#/type"),
              UnionTypeKeyword(
                Seq(
                  WithLocation(uri("#/type"), NullTypeKeyword),
                  WithLocation(uri("#/type"), StringTypeKeyword)
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
      assertKeywords(schema) { keywords =>
        assertEquals(
          keywords.keywords,
          Seq(
            WithLocation(uri("#/type"), StringTypeKeyword),
            WithLocation(
              uri("#/enum"),
              EnumKeyword(
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
      assertKeywords(schema) { keywords =>
        assertEquals(
          keywords.keywords,
          Seq(
            WithLocation(uri("#/type"), StringTypeKeyword),
            WithLocation(
              uri("#/const"),
              EnumKeyword(
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
      assertKeywords(schema) { keywords =>
        assertEquals(
          assertable(keywords).keywords,
          Seq(
            WithLocation(uri("https://example.net/root.json#/type"), ArrayTypeKeyword),
            WithLocation(
              uri("https://example.net/root.json#/items"),
              ArrayItemsKeyword(
                items = Some(
                  value = Keywords(
                    vocabularyForTest,
                    schema = SchemaValue(
                      value = ObjectValue(
                        properties = Map(
                          "$ref" -> StringValue(
                            value = "#item"
                          )
                        )
                      )
                    ),
                    keywords = List(
                      WithLocation(
                        uri("https://example.net/root.json#/items/$ref"),
                        LazyParseKeywords(uri("https://example.net/root.json#item"), assertableResolve)
                      )
                    ),
                    ignored = Set()
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
      assertKeywords(schema) { keywords =>
        assertEquals(
          keywords.keywords.size,
          1
        )
      }
    }
  }

  test("subitem $ref/$def") {
    withSchema(subItemRefDefsSchema) { schema =>
      assertKeywords(schema) { keywords =>
        assertEquals(
          keywords.keywords.length,
          2
        )
      }
    }
  }
}
