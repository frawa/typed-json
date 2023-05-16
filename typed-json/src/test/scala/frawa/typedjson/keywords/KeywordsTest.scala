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
          Set(WithLocation(NullTypeKeyword, KeywordLocation(Pointer.parse("/type").get)))
        )
      }
    }
  }

  test("boolean") {
    withSchema(boolSchema) { schema =>
      assertKeywords(schema) { keywords =>
        assertEquals(
          keywords.keywords,
          Set(WithLocation(BooleanTypeKeyword, KeywordLocation(Pointer.parse("/type").get)))
        )
      }
    }
  }

  test("true schema") {
    withSchema(trueSchema) { schema =>
      assertKeywords(schema) { keywords =>
        assertEquals(keywords.keywords, Set(WithLocation(TrivialKeyword(true))))
      }
    }
  }

  test("false schema") {
    withSchema(falseSchema) { schema =>
      assertKeywords(schema) { keywords =>
        assertEquals(keywords.keywords, Set(WithLocation(TrivialKeyword(false))))
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
              NotKeyword(
                Keywords(
                  vocabularyForTest,
                  Set(WithLocation(TrivialKeyword(false), KeywordLocation(Pointer.parse("/not").get)))
                )
              ),
              KeywordLocation(Pointer.parse("/not").get)
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
        assertEquals(keywords.keywords, Set(WithLocation(TrivialKeyword(true))))
      }
    }
  }

  test("string") {
    withSchema(stringSchema) { schema =>
      assertKeywords(schema) { keywords =>
        assertEquals(
          keywords.keywords,
          Set(WithLocation(StringTypeKeyword, KeywordLocation(Pointer.parse("/type").get)))
        )
      }
    }
  }

  test("number") {
    withSchema(numberSchema) { schema =>
      assertKeywords(schema) { keywords =>
        assertEquals(
          keywords.keywords,
          Set(WithLocation(NumberTypeKeyword, KeywordLocation(Pointer.parse("/type").get)))
        )
      }
    }
  }

  test("array") {
    withSchema(arraySchema) { schema =>
      assertKeywords(schema) { keywords =>
        assertEquals(
          keywords.keywords,
          Set(WithLocation(ArrayTypeKeyword, KeywordLocation(Pointer.parse("/type").get)))
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
            WithLocation(ArrayTypeKeyword, KeywordLocation(Pointer.parse("/type").get)),
            WithLocation(
              ArrayItemsKeyword(
                Some(
                  Keywords(
                    vocabularyForTest,
                    Set(
                      WithLocation(
                        NumberTypeKeyword,
                        KeywordLocation(Pointer.parse("/items/type").get)
                      )
                    )
                  )
                )
              ),
              KeywordLocation(Pointer.parse("/items").get)
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
              ArrayItemsKeyword(
                Some(
                  Keywords(
                    vocabularyForTest,
                    Set(
                      WithLocation(
                        NumberTypeKeyword,
                        KeywordLocation(Pointer.parse("/items/type").get)
                      )
                    )
                  )
                )
              ),
              KeywordLocation(Pointer.parse("/items").get)
            ),
            WithLocation(ArrayTypeKeyword, KeywordLocation(Pointer.parse("/type").get))
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
            WithLocation(ObjectTypeKeyword, KeywordLocation(Pointer.parse("/type").get)),
            WithLocation(
              ObjectPropertiesKeyword(
                Map(
                  "toto" -> Keywords(
                    vocabularyForTest,
                    keywords = Set(
                      WithLocation(
                        NumberTypeKeyword,
                        KeywordLocation(Pointer.parse("/properties/toto/type").get)
                      )
                    )
                  ),
                  "titi" -> Keywords(
                    vocabularyForTest,
                    keywords = Set(
                      WithLocation(
                        StringTypeKeyword,
                        KeywordLocation(Pointer.parse("/properties/titi/type").get)
                      )
                    )
                  )
                )
              ),
              KeywordLocation(Pointer.parse("/properties").get)
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
            WithLocation(ObjectTypeKeyword, KeywordLocation(Pointer.parse("/type").get)),
            WithLocation(
              ObjectRequiredKeyword(
                names = IndexedSeq(
                  "titi"
                )
              ),
              KeywordLocation(Pointer.parse("/required").get)
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
              AllOfKeyword(
                Seq(
                  Keywords(
                    vocabularyForTest,
                    keywords = Set(
                      WithLocation(
                        NumberTypeKeyword,
                        KeywordLocation(Pointer.parse("/allOf/0/type").get)
                      )
                    )
                  )
                )
              ),
              KeywordLocation(Pointer.parse("/allOf").get)
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
              AnyOfKeyword(
                Seq(
                  Keywords(
                    vocabularyForTest,
                    keywords = Set(
                      WithLocation(
                        NumberTypeKeyword,
                        KeywordLocation(Pointer.parse("/anyOf/0/type").get)
                      )
                    )
                  ),
                  Keywords(
                    vocabularyForTest,
                    keywords = Set(
                      WithLocation(
                        StringTypeKeyword,
                        KeywordLocation(Pointer.parse("/anyOf/1/type").get)
                      )
                    )
                  )
                )
              ),
              KeywordLocation(Pointer.parse("/anyOf").get)
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
              OneOfKeyword(
                Seq(
                  Keywords(
                    vocabularyForTest,
                    keywords = Set(
                      WithLocation(
                        NumberTypeKeyword,
                        KeywordLocation(Pointer.parse("/oneOf/0/type").get)
                      )
                    )
                  ),
                  Keywords(
                    vocabularyForTest,
                    keywords = Set(
                      WithLocation(
                        StringTypeKeyword,
                        KeywordLocation(Pointer.parse("/oneOf/1/type").get)
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
              IfThenElseKeyword(
                Some(
                  Keywords(
                    vocabularyForTest,
                    Set(
                      WithLocation(NumberTypeKeyword, KeywordLocation(Pointer.empty / "if" / "type"))
                    ),
                    Seq()
                  )
                ),
                Some(
                  Keywords(
                    vocabularyForTest,
                    Set(
                      WithLocation(
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
              UnionTypeKeyword(
                Seq(
                  WithLocation(NullTypeKeyword, KeywordLocation(Pointer.empty / "type")),
                  WithLocation(StringTypeKeyword, KeywordLocation(Pointer.empty / "type"))
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
            WithLocation(StringTypeKeyword, KeywordLocation(Pointer.empty / "type")),
            WithLocation(
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
            WithLocation(StringTypeKeyword, KeywordLocation(Pointer.empty / "type")),
            WithLocation(
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
