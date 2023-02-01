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

package frawa.typedjson.eval

import munit.FunSuite

import frawa.typedjson.testutil.TestSchemas.*
import frawa.typedjson.testutil.TestUtil.{*, given}
import frawa.typedjson.keywords.Keywords
import frawa.typedjson.keywords._
import frawa.typedjson.eval._
import frawa.typedjson.pointer.Pointer
import frawa.typedjson.validation._
import frawa.typedjson.parser.Value
import frawa.typedjson.parser.Value._
import scala.reflect.TypeTest

class EvalTest extends FunSuite:
  import Util.{_, given}

  val evalFlag  = Eval[MyR, FlagOutput]
  val evalBasic = Eval[MyResult, BasicOutput]

  given Eval[MyResult, BasicOutput] = evalBasic

  test("null") {
    given Eval[MyR, FlagOutput] = evalFlag
    withCompiledSchema(nullSchema) { fun =>
      assertEquals(fun(NullValue), FlagOutput(true))
      assertEquals(fun(BoolValue(true)), FlagOutput(false))
    }
  }

  test("true") {
    given Eval[MyR, FlagOutput] = evalFlag
    withCompiledSchema(trueSchema) { fun =>
      assertEquals(fun(BoolValue(true)), FlagOutput(true))
      assertEquals(fun(NullValue), FlagOutput(true))
    }
  }

  test("null with errors") {
    withCompiledSchema(nullSchema) { fun =>
      assertEquals(fun(NullValue), BasicOutput(true, Seq()))
      assertEquals(
        fun(BoolValue(true)),
        BasicOutput(
          false,
          Seq(WithPointer(TypeMismatch("null")))
        )
      )
    }
  }

  test("false") {
    given Eval[MyR, FlagOutput] = evalFlag
    withCompiledSchema(falseSchema) { fun =>
      assertEquals(fun(BoolValue(true)), FlagOutput(false))
      assertEquals(fun(NullValue), FlagOutput(false))
      assertEquals(fun(parseJsonValue("13")), FlagOutput(false))
    }
  }

  test("false with errors") {
    withCompiledSchema(falseSchema) { fun =>
      assertEquals(fun(parseJsonValue("{}")), BasicOutput(false, Seq(WithPointer(FalseSchemaReason()))))
    }
  }

  test("not false") {
    given Eval[MyR, FlagOutput] = evalFlag
    withCompiledSchema(notFalseSchema) { fun =>
      assertEquals(fun(parseJsonValue("null")), FlagOutput(true))
      assertEquals(fun(parseJsonValue("13")), FlagOutput(true))
      assertEquals(fun(parseJsonValue("{}")), FlagOutput(true))
    }
  }

  test("not empty with errors") {
    withCompiledSchema("""{"not": {}}""") { fun =>
      assertEquals(fun(parseJsonValue("null")), BasicOutput(false, Seq(WithPointer(NotInvalid()))))
      assertEquals(fun(parseJsonValue("13")), BasicOutput(false, Seq(WithPointer(NotInvalid()))))
      assertEquals(fun(parseJsonValue("{}")), BasicOutput(false, Seq(WithPointer(NotInvalid()))))
    }
  }

  test("array items") {
    withCompiledSchema(numberArraySchema) { fun =>
      assertEquals(fun(parseJsonValue("null")), BasicOutput(false, Seq(WithPointer(TypeMismatch("array")))))
      assertEquals(fun(parseJsonValue("[13]")), BasicOutput(true, Seq()))
      assertEquals(
        fun(parseJsonValue("[true]")),
        BasicOutput(false, Seq(WithPointer(TypeMismatch("number"), Pointer.empty / 0)))
      )
    }
  }

  test("object") {
    withCompiledSchema(totoObjectSchema) { fun =>
      assertEquals(
        fun(parseJsonValue("""{
                             |"toto": 13,
                             |"titi": "hello"
                             |}
                             |""".stripMargin)),
        BasicOutput(true, Seq())
      )
      assertEquals(
        fun(parseJsonValue("null")),
        BasicOutput(false, Seq(WithPointer(TypeMismatch("object"))))
      )
    }
  }

  test("object with pointer") {
    withCompiledSchema(totoObjectSchema) { fun =>
      assertEquals(
        fun(parseJsonValue("""{"toto": 13,"titi": true}""")),
        BasicOutput(false, Seq(WithPointer(TypeMismatch("string"), Pointer.empty / "titi")))
      )
    }
  }

  test("object missing property") {
    withCompiledSchema(totoObjectSchema) { fun =>
      assertEquals(
        fun(parseJsonValue("""{"toto": 13}""")),
        BasicOutput(true, Seq())
      )
    }
  }

  test("object missing required property") {
    withCompiledSchema("""{
                         |"type": "object",
                         |"properties": {
                         |  "toto": { "type": "number" },
                         |  "titi": { "type": "string" }
                         |},
                         |"required": ["titi"]
                         |}
                         |""".stripMargin) { fun =>
      assertEquals(
        fun(parseJsonValue("""{"toto": 13}""")),
        BasicOutput(false, Seq(WithPointer(MissingRequiredProperties(Seq("titi")))))
      )
    }
  }

  test("all of") {
    withCompiledSchema(allOfSchema) { fun =>
      assertEquals(
        fun(parseJsonValue("13")),
        BasicOutput(true, Seq())
      )
    }
  }

  test("impossible all of") {
    withCompiledSchema("""{
                         |"allOf": [
                         |  { "type": "number" },
                         |  { "type": "string" }
                         |]
                         |}
                         |""".stripMargin) { fun =>
      assertEquals(
        fun(parseJsonValue("1313")),
        BasicOutput(false, Seq(WithPointer(TypeMismatch("string"))))
      )
    }
  }

  test("any of") {
    withCompiledSchema(anyOfSchema) { fun =>
      assertEquals(
        fun(parseJsonValue("1313")),
        BasicOutput(true, Seq())
      )
      assertEquals(
        fun(parseJsonValue("true")),
        BasicOutput(
          false,
          Seq(
            WithPointer(TypeMismatch("number")),
            WithPointer(TypeMismatch("string"))
          )
        )
      )
    }
  }

  test("one of") {
    withCompiledSchema(oneOfSchema) { fun =>
      assertEquals(
        fun(parseJsonValue("1313")),
        BasicOutput(true, Seq())
      )
    }
  }

  test("failed one of: none") {
    withCompiledSchema("""{
                         |"oneOf": [
                         |  { "type": "string" },
                         |  { "type": "boolean" }
                         |]
                         |}
                         |""".stripMargin) { fun =>
      assertEquals(
        fun(parseJsonValue("1313")),
        BasicOutput(false, Seq(WithPointer(TypeMismatch("string")), WithPointer(TypeMismatch("boolean"))))
      )
    }
  }

  test("failed one of: two") {
    withCompiledSchema("""{
                         |"oneOf": [
                         |  { "type": "number" },
                         |  { "type": "number" }
                         |]
                         |}
                         |""".stripMargin) { fun =>
      assertEquals(
        fun(parseJsonValue("1313")),
        BasicOutput(false, Seq(WithPointer(NotOneOf(2))))
      )
    }
  }

  test("not") {
    withCompiledSchema("""{"not": { "type": "number" }}""") { fun =>
      assertEquals(
        fun(parseJsonValue("true")),
        BasicOutput(true, Seq())
      )
      assertEquals(
        fun(parseJsonValue("1313")),
        BasicOutput(false, Seq(WithPointer(NotInvalid())))
      )
    }
  }

object Util:
  private val vocabularyForTest = dialect(Seq(Vocabulary.coreId, Vocabulary.validationId, Vocabulary.applicatorId))

  def withKeywords(schema: SchemaValue)(f: Keywords => Unit): Unit =
    Keywords(schema, vocabularyForTest, None)
      .fold(
        errors => throw new IllegalArgumentException(s"no keywords: $errors"),
        keywords => f(keywords)
      )

  def withCompiledSchema[R[_], O](schema: String)(using eval: Eval[R, O])(f: R[Value => O] => Unit): Unit =
    withSchema(schema) { schema =>
      withKeywords(schema) { keywords =>
        val fun = eval.fun(eval.compile(keywords))
        f(fun)
      }
    }

  type MyR[O] = O

  given TheResultMonad[MyR] with
    def unit[A](a: A): MyR[A]                         = a
    def bind[A, B](a: MyR[A])(f: A => MyR[B]): MyR[B] = f(a)
    def output[O](result: MyR[O]): O                  = result

  type MyResult[O] = MyR[O]

  // given TheResultMonad[MyResult] with
  //   def unit[A](a: A): MyResult[A]                                      = a
  //   def flatMap[A, B](a: MyResult[A])(f: A => MyResult[B]): MyResult[B] = f(a)
  //   def output[O](result: MyResult[O]): O                               = result
