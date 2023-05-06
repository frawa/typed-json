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
import frawa.typedjson.parser.Value.*
import frawa.typedjson.pointer.Pointer
import frawa.typedjson.testutil.TestSchemas.*
import frawa.typedjson.testutil.TestUtil.{*, given}
import frawa.typedjson.util.UriUtil.uri
import munit.FunSuite
import frawa.typedjson.util.WithPointer

class KeywordsTest extends FunSuite:
  private val vocabularyForTest = dialect(Seq(Vocabulary.coreId, Vocabulary.validationId, Vocabulary.applicatorId)).get

  private def assertKeywords(schema: SchemaValue)(
      f: Keywords => Unit
  ): Either[Nothing, Unit] =
    val resolver: LoadedSchemasResolver = LoadedSchemasResolver(schema)
    val scope                           = DynamicScope.empty
    val withParsed =
      for keywords <- Keywords.parseKeywords(vocabularyForTest, resolver.push(schema), scope)
      yield f(keywords)
    withParsed.swap
      .map(messages => fail("parsing keywords failed", clues(clue[SchemaProblems](messages))))
      .swap

  private def assertSchemaProblems(schema: SchemaValue)(
      f: SchemaProblems => Unit
  ): Unit =
    val resolver: LoadedSchemasResolver = LoadedSchemasResolver(schema)
    val scope                           = DynamicScope.empty
    Keywords.parseKeywords(vocabularyForTest, resolver.push(schema), scope) match
      case Right(_)     => fail("parsing keywords expected to fail")
      case Left(errors) => f(errors)

  test("null") {
    withSchema(nullSchema) { schema =>
      assertKeywords(schema) { keywords =>
        assertEquals(
          keywords.keywords,
          Set(WithLocation(uri("#/type"), NullTypeKeyword, KeywordLocation(Pointer.parse("/type"))))
        )
      }
    }
  }

  test("boolean") {
    withSchema(boolSchema) { schema =>
      assertKeywords(schema) { keywords =>
        assertEquals(
          keywords.keywords,
          Set(WithLocation(uri("#/type"), BooleanTypeKeyword, KeywordLocation(Pointer.parse("/type"))))
        )
      }
    }
  }

  test("true schema") {
    withSchema(trueSchema) { schema =>
      assertKeywords(schema) { keywords =>
        assertEquals(keywords.keywords, Set(WithLocation(uri("#"), TrivialKeyword(true))))
      }
    }
  }

  test("false schema") {
    withSchema(falseSchema) { schema =>
      assertKeywords(schema) { keywords =>
        assertEquals(keywords.keywords, Set(WithLocation(uri("#"), TrivialKeyword(false))))
      }
    }
  }

  test("not false") {
    withSchema(notFalseSchema) { schema =>
      assertKeywords(schema) { keywords =>
        assertEquals(
          keywords.keywords,
          Set(
            WithLocation(
              uri("#/not"),
              NotKeyword(
                Keywords(
                  vocabularyForTest,
                  Set(WithLocation(uri("#/not"), TrivialKeyword(false), KeywordLocation(Pointer.parse("/not"))))
                )
              ),
              KeywordLocation(Pointer.parse("/not"))
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
        assertEquals(keywords.keywords, Set(WithLocation(uri("#"), TrivialKeyword(true))))
      }
    }
  }

  test("string") {
    withSchema(stringSchema) { schema =>
      assertKeywords(schema) { keywords =>
        assertEquals(
          keywords.keywords,
          Set(WithLocation(uri("#/type"), StringTypeKeyword, KeywordLocation(Pointer.parse("/type"))))
        )
      }
    }
  }

  test("number") {
    withSchema(numberSchema) { schema =>
      assertKeywords(schema) { keywords =>
        assertEquals(
          keywords.keywords,
          Set(WithLocation(uri("#/type"), NumberTypeKeyword, KeywordLocation(Pointer.parse("/type"))))
        )
      }
    }
  }

  test("array") {
    withSchema(arraySchema) { schema =>
      assertKeywords(schema) { keywords =>
        assertEquals(
          keywords.keywords,
          Set(WithLocation(uri("#/type"), ArrayTypeKeyword, KeywordLocation(Pointer.parse("/type"))))
        )
      }
    }
  }

  test("array with items") {
    withSchema(numberArraySchema) { schema =>
      assertKeywords(schema) { keywords =>
        assertEquals(
          keywords.keywords,
          Set(
            WithLocation(uri("#/type"), ArrayTypeKeyword, KeywordLocation(Pointer.parse("/type"))),
            WithLocation(
              uri("#/items"),
              ArrayItemsKeyword(
                Some(
                  Keywords(
                    vocabularyForTest,
                    Set(
                      WithLocation(
                        uri("#/items/type"),
                        NumberTypeKeyword,
                        KeywordLocation(Pointer.parse("/items/type"))
                      )
                    )
                  )
                )
              ),
              KeywordLocation(Pointer.parse("/items"))
            )
          )
        )
      }
    }
    withSchema("""{"items": { "type": "number"}, "type": "array"}""") { schema =>
      assertKeywords(schema) { keywords =>
        assertEquals(
          keywords.keywords,
          Set(
            WithLocation(
              uri("#/items"),
              ArrayItemsKeyword(
                Some(
                  Keywords(
                    vocabularyForTest,
                    Set(
                      WithLocation(
                        uri("#/items/type"),
                        NumberTypeKeyword,
                        KeywordLocation(Pointer.parse("/items/type"))
                      )
                    )
                  )
                )
              ),
              KeywordLocation(Pointer.parse("/items"))
            ),
            WithLocation(uri("#/type"), ArrayTypeKeyword, KeywordLocation(Pointer.parse("/type")))
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
          Set(
            WithLocation(uri("#/type"), ObjectTypeKeyword, KeywordLocation(Pointer.parse("/type"))),
            WithLocation(
              uri("#/properties"),
              ObjectPropertiesKeyword(
                Map(
                  "toto" -> Keywords(
                    vocabularyForTest,
                    keywords = Set(
                      WithLocation(
                        uri("#/properties/toto/type"),
                        NumberTypeKeyword,
                        KeywordLocation(Pointer.parse("/properties/toto/type"))
                      )
                    )
                  ),
                  "titi" -> Keywords(
                    vocabularyForTest,
                    keywords = Set(
                      WithLocation(
                        uri("#/properties/titi/type"),
                        StringTypeKeyword,
                        KeywordLocation(Pointer.parse("/properties/titi/type"))
                      )
                    )
                  )
                )
              ),
              KeywordLocation(Pointer.parse("/properties"))
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
          Set(
            WithLocation(uri("#/type"), ObjectTypeKeyword, KeywordLocation(Pointer.parse("/type"))),
            WithLocation(
              uri("#/required"),
              ObjectRequiredKeyword(
                names = IndexedSeq(
                  "titi"
                )
              ),
              KeywordLocation(Pointer.parse("/required"))
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
          Set(
            WithLocation(
              uri("#/allOf"),
              AllOfKeyword(
                Seq(
                  Keywords(
                    vocabularyForTest,
                    keywords = Set(
                      WithLocation(
                        uri("#/allOf/0/type"),
                        NumberTypeKeyword,
                        KeywordLocation(Pointer.parse("/allOf/0/type"))
                      )
                    )
                  )
                )
              ),
              KeywordLocation(Pointer.parse("/allOf"))
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
          Set(
            WithLocation(
              uri("#/anyOf"),
              AnyOfKeyword(
                Seq(
                  Keywords(
                    vocabularyForTest,
                    keywords = Set(
                      WithLocation(
                        uri("#/anyOf/0/type"),
                        NumberTypeKeyword,
                        KeywordLocation(Pointer.parse("/anyOf/0/type"))
                      )
                    )
                  ),
                  Keywords(
                    vocabularyForTest,
                    keywords = Set(
                      WithLocation(
                        uri("#/anyOf/1/type"),
                        StringTypeKeyword,
                        KeywordLocation(Pointer.parse("/anyOf/1/type"))
                      )
                    )
                  )
                )
              ),
              KeywordLocation(Pointer.parse("/anyOf"))
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
          Set(
            WithLocation(
              uri("#/oneOf"),
              OneOfKeyword(
                Seq(
                  Keywords(
                    vocabularyForTest,
                    keywords = Set(
                      WithLocation(
                        uri("#/oneOf/0/type"),
                        NumberTypeKeyword,
                        KeywordLocation(Pointer.parse("/oneOf/0/type"))
                      )
                    )
                  ),
                  Keywords(
                    vocabularyForTest,
                    keywords = Set(
                      WithLocation(
                        uri("#/oneOf/1/type"),
                        StringTypeKeyword,
                        KeywordLocation(Pointer.parse("/oneOf/1/type"))
                      )
                    )
                  )
                )
              ),
              KeywordLocation(Pointer.empty / "oneOf")
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
          Set(
            WithLocation(
              uri("#/else"),
              IfThenElseKeyword(
                Some(
                  Keywords(
                    vocabularyForTest,
                    Set(
                      WithLocation(uri("#/if/type"), NumberTypeKeyword, KeywordLocation(Pointer.empty / "if" / "type"))
                    ),
                    Seq()
                  )
                ),
                Some(
                  Keywords(
                    vocabularyForTest,
                    Set(
                      WithLocation(
                        uri("#/then/type"),
                        NumberTypeKeyword,
                        KeywordLocation(Pointer.empty / "then" / "type")
                      )
                    ),
                    Seq()
                  )
                ),
                Some(
                  Keywords(
                    vocabularyForTest,
                    Set(
                      WithLocation(
                        uri("#/else/type"),
                        StringTypeKeyword,
                        KeywordLocation(Pointer.empty / "else" / "type")
                      )
                    ),
                    Seq()
                  )
                )
              ),
              KeywordLocation(Pointer.empty / "else")
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
          Set(
            WithLocation(
              uri("#/type"),
              UnionTypeKeyword(
                Seq(
                  WithLocation(uri("#/type"), NullTypeKeyword, KeywordLocation(Pointer.empty / "type")),
                  WithLocation(uri("#/type"), StringTypeKeyword, KeywordLocation(Pointer.empty / "type"))
                )
              ),
              KeywordLocation(Pointer.empty / "type")
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
          Set(
            WithLocation(uri("#/type"), StringTypeKeyword, KeywordLocation(Pointer.empty / "type")),
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
              ),
              kl = KeywordLocation(Pointer.empty / "enum")
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
          Set(
            WithLocation(uri("#/type"), StringTypeKeyword, KeywordLocation(Pointer.empty / "type")),
            WithLocation(
              uri("#/const"),
              EnumKeyword(
                values = Seq(
                  StringValue(
                    value = "first"
                  )
                )
              ),
              kl = KeywordLocation(Pointer.empty / "const")
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
          keywords.keywords.size,
          2
        )
      }
    }
  }
