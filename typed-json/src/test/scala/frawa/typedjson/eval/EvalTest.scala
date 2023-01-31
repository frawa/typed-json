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

  val eval  = Eval[MyR, MyO]
  val eval2 = Eval[MyResult, MyOutput]

  test("null") {
    given Eval[MyR, MyO] = eval
    withCompiledSchema(nullSchema) { fun =>
      assertEquals(fun(NullValue), MyO(true))
      assertEquals(fun(BoolValue(true)), MyO(false))
    }
  }

  test("true") {
    given Eval[MyR, MyO] = eval
    withCompiledSchema(trueSchema) { fun =>
      assertEquals(fun(BoolValue(true)), MyO(true))
      assertEquals(fun(NullValue), MyO(true))
    }
  }

  test("null with errors") {
    given Eval[MyResult, MyOutput] = eval2
    withCompiledSchema(nullSchema) { fun =>
      assertEquals(fun(NullValue), MyOutput(true, Seq()))
      assertEquals(
        fun(BoolValue(true)),
        MyOutput(
          false,
          Seq(WithPointer(TypeMismatch("null")))
        )
      )
    }
  }

  test("false") {
    given Eval[MyR, MyO] = eval
    withCompiledSchema(falseSchema) { fun =>
      assertEquals(fun(BoolValue(true)), MyO(false))
      assertEquals(fun(NullValue), MyO(false))
      assertEquals(fun(parseJsonValue("13")), MyO(false))
    }
  }

  test("false with errors") {
    given Eval[MyResult, MyOutput] = eval2
    withCompiledSchema(falseSchema) { fun =>
      assertEquals(fun(parseJsonValue("{}")), MyOutput(false, Seq(WithPointer(FalseSchemaReason()))))
    }
  }

  test("not false") {
    given Eval[MyR, MyO] = eval
    withCompiledSchema(notFalseSchema) { fun =>
      assertEquals(fun(parseJsonValue("null")), MyO(true))
      assertEquals(fun(parseJsonValue("13")), MyO(true))
      assertEquals(fun(parseJsonValue("{}")), MyO(true))
    }
  }

  test("not empty with errors") {
    given Eval[MyResult, MyOutput] = eval2
    withCompiledSchema("""{"not": {}}""") { fun =>
      assertEquals(fun(parseJsonValue("null")), MyOutput(false, Seq(WithPointer(NotInvalid()))))
      assertEquals(fun(parseJsonValue("13")), MyOutput(false, Seq(WithPointer(NotInvalid()))))
      assertEquals(fun(parseJsonValue("{}")), MyOutput(false, Seq(WithPointer(NotInvalid()))))
    }
  }

  test("array items") {
    given Eval[MyResult, MyOutput] = eval2
    withCompiledSchema(numberArraySchema) { fun =>
      assertEquals(fun(parseJsonValue("null")), MyOutput(false, Seq(WithPointer(TypeMismatch("array")))))
      assertEquals(fun(parseJsonValue("[13]")), MyOutput(true, Seq()))
      assertEquals(
        fun(parseJsonValue("[true]")),
        MyOutput(false, Seq(WithPointer(TypeMismatch("number"), Pointer.empty / 0)))
      )
    }
  }

  test("object") {
    given Eval[MyResult, MyOutput] = eval2
    withCompiledSchema(totoObjectSchema) { fun =>
      assertEquals(
        fun(parseJsonValue("""{
                             |"toto": 13,
                             |"titi": "hello"
                             |}
                             |""".stripMargin)),
        MyOutput(true, Seq())
      )
      assertEquals(
        fun(parseJsonValue("null")),
        MyOutput(false, Seq(WithPointer(TypeMismatch("object"))))
      )
    }
  }

  test("object with pointer") {
    given Eval[MyResult, MyOutput] = eval2
    withCompiledSchema(totoObjectSchema) { fun =>
      assertEquals(
        fun(parseJsonValue("""{"toto": 13,"titi": true}""")),
        MyOutput(false, Seq(WithPointer(TypeMismatch("string"), Pointer.empty / "titi")))
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
  case class MyO(valid: Boolean)

  given TheResultMonad[MyR] with
    def unit[A](a: A): MyR[A]                         = a
    def bind[A, B](a: MyR[A])(f: A => MyR[B]): MyR[B] = f(a)
    def output[O](result: MyR[O]): O                  = result

  given OutputOps[MyO] with
    def valid: MyO                                                     = MyO(true)
    def valid(annotation: ValidationAnnotation, pointer: Pointer): MyO = MyO(true)
    def invalid(error: ValidationError, pointer: Pointer): MyO         = MyO(false)
    def invalid(problems: SchemaProblems): MyO                         = MyO(false)

    def all(os: Seq[MyO]): MyO                                                            = MyO(os.forall(_.valid))
    def any(os: Seq[MyO]): MyO                                                            = ???
    def one(os: Seq[MyO]): MyO                                                            = ???
    def contains(os: Seq[MyO], min: Option[Int], max: Option[Int], pointer: Pointer): MyO = ???

    extension (o: MyO)
      def not: MyO         = o.copy(valid = !o.valid)
      def isValid: Boolean = ???
    //   def combine(o2: MyO): MyO = all(Seq(o, o2))

  type MyResult[O]   = MyR[O]
  type MyOutputError = WithPointer[ValidationError]
  // TODO this will converge to "basic" output format,
  // see https://json-schema.org/draft/2020-12/json-schema-core.html#name-basic
  case class MyOutput(valid: Boolean, errors: Seq[MyOutputError])

  // given TheResultMonad[MyResult] with
  //   def unit[A](a: A): MyResult[A]                                      = a
  //   def flatMap[A, B](a: MyResult[A])(f: A => MyResult[B]): MyResult[B] = f(a)
  //   def output[O](result: MyResult[O]): O                               = result

  given OutputOps[MyOutput] with
    def valid: MyOutput                                                     = MyOutput(true, Seq())
    def valid(annotation: ValidationAnnotation, pointer: Pointer): MyOutput = MyOutput(true, Seq())
    def invalid(error: ValidationError, pointer: Pointer): MyOutput = MyOutput(false, Seq(WithPointer(error, pointer)))
    def invalid(problems: SchemaProblems): MyOutput                 = MyOutput(false, Seq())

    def all(os: Seq[MyOutput]): MyOutput = MyOutput(os.forall(_.valid), os.flatMap(_.errors))
    def any(os: Seq[MyOutput]): MyOutput = ???
    def one(os: Seq[MyOutput]): MyOutput = ???
    def contains(os: Seq[MyOutput], min: Option[Int], max: Option[Int], pointer: Pointer): MyOutput = ???

    extension (o: MyOutput)
      def not: MyOutput =
        if o.valid then o.copy(valid = false, errors = Seq(WithPointer(NotInvalid())))
        else o.copy(valid = true, errors = Seq())
      def isValid: Boolean = ???
