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

import frawa.typedjson.eval.*
import frawa.typedjson.keywords.*
import frawa.typedjson.keywords.SchemaProblems.MissingReference
import frawa.typedjson.meta.MetaSchemas
import frawa.typedjson.parser.Value
import frawa.typedjson.parser.Value.*
import frawa.typedjson.pointer.Pointer
import frawa.typedjson.testutil.TestSchemas.*
import frawa.typedjson.testutil.TestUtil.{*, given}
import frawa.typedjson.validation.*
import munit.FunSuite

import scala.reflect.TypeTest

class EvalTest extends FunSuite:
  import Util.{*, given}
  // import FlagOutput.given
  // import BasicOutput.given

  val evalFlag  = Eval[MyR[FlagOutput], FlagOutput]
  val evalBasic = Eval[MyR[BasicOutput], BasicOutput]

  given Eval[MyR[BasicOutput], BasicOutput] = evalBasic

  private def doApply[O: OutputOps](fun: MyR[O][Value => O], value: Value)(using resolver: SchemaResolver): O =
    val (o, s) = fun.map(_(value))(myZero(resolver))
    // println(s"counted ${s.count} binds")
    o

  private def doApplyBulk[O: OutputOps](fun: MyR[O][Value => O], values: Seq[Value], fun2: MyState => Unit)(using
      resolver: SchemaResolver
  ): Seq[O] =
    val (s, os) = values
      .foldLeft((myZero(resolver), Seq.empty[O])) { case ((state, os), v) =>
        val (o, s) = fun.map(_(v))(state)
        (s, os :+ o)
      }
    // println(s"state cached resolutions ${s.resolved.keySet} ${s.hits}")
    fun2(s)
    os

  test("null") {
    given Eval[MyR[FlagOutput], FlagOutput] = evalFlag
    withCompiledSchema(nullSchema) { fun =>
      assertEquals(doApply(fun, NullValue), FlagOutput(true))
      assertEquals(doApply(fun, BoolValue(true)), FlagOutput(false))
    }
  }

  test("true") {
    given Eval[MyR[FlagOutput], FlagOutput] = evalFlag
    withCompiledSchema(trueSchema) { fun =>
      assertEquals(doApply(fun, BoolValue(true)), FlagOutput(true))
      assertEquals(doApply(fun, NullValue), FlagOutput(true))
    }
  }

  test("null with errors") {
    withCompiledSchema(nullSchema) { fun =>
      assertEquals(doApply(fun, NullValue), BasicOutput(true, Seq()))
      assertEquals(
        doApply(fun, BoolValue(true)),
        BasicOutput(
          false,
          Seq(WithPointer(TypeMismatch("null")))
        )
      )
    }
  }

  test("false") {
    given Eval[MyR[FlagOutput], FlagOutput] = evalFlag
    withCompiledSchema(falseSchema) { fun =>
      assertEquals(doApply(fun, BoolValue(true)), FlagOutput(false))
      assertEquals(doApply(fun, NullValue), FlagOutput(false))
      assertEquals(doApply(fun, parseJsonValue("13")), FlagOutput(false))
    }
  }

  test("false with errors") {
    withCompiledSchema(falseSchema) { fun =>
      assertEquals(doApply(fun, parseJsonValue("{}")), BasicOutput(false, Seq(WithPointer(FalseSchemaReason()))))
    }
  }

  test("boolean") {
    withCompiledSchema(boolSchema) { fun =>
      assertEquals(doApply(fun, parseJsonValue("true")), BasicOutput(true, Seq()))
      assertEquals(doApply(fun, parseJsonValue("13")), BasicOutput(false, Seq(WithPointer(TypeMismatch("boolean")))))
    }
  }

  test("true schema") {
    withCompiledSchema(trueSchema) { fun =>
      assertEquals(doApply(fun, parseJsonValue("null")), BasicOutput(true, Seq()))
      assertEquals(doApply(fun, parseJsonValue("13")), BasicOutput(true, Seq()))
      assertEquals(doApply(fun, parseJsonValue("{}")), BasicOutput(true, Seq()))
    }
  }

  test("false schema") {
    withCompiledSchema(falseSchema) { fun =>
      assertEquals(doApply(fun, parseJsonValue("null")), BasicOutput(false, Seq(WithPointer(FalseSchemaReason()))))
      assertEquals(doApply(fun, parseJsonValue("13")), BasicOutput(false, Seq(WithPointer(FalseSchemaReason()))))
      assertEquals(doApply(fun, parseJsonValue("{}")), BasicOutput(false, Seq(WithPointer(FalseSchemaReason()))))
    }
  }

  test("not false") {
    given Eval[MyR[FlagOutput], FlagOutput] = evalFlag
    withCompiledSchema(notFalseSchema) { fun =>
      assertEquals(doApply(fun, parseJsonValue("null")), FlagOutput(true))
      assertEquals(doApply(fun, parseJsonValue("13")), FlagOutput(true))
      assertEquals(doApply(fun, parseJsonValue("{}")), FlagOutput(true))
    }
  }

  test("empty schema") {
    withCompiledSchema(emtpySchema) { fun =>
      assertEquals(doApply(fun, parseJsonValue("null")), BasicOutput(true, Seq()))
      assertEquals(doApply(fun, parseJsonValue("13")), BasicOutput(true, Seq()))
      assertEquals(doApply(fun, parseJsonValue("{}")), BasicOutput(true, Seq()))
    }
  }

  test("not empty schema") {
    withCompiledSchema("""{"not": {}}""") { fun =>
      assertEquals(doApply(fun, parseJsonValue("null")), BasicOutput(false, Seq(WithPointer(NotInvalid()))))
      assertEquals(doApply(fun, parseJsonValue("13")), BasicOutput(false, Seq(WithPointer(NotInvalid()))))
      assertEquals(doApply(fun, parseJsonValue("{}")), BasicOutput(false, Seq(WithPointer(NotInvalid()))))
    }
  }

  test("string") {
    withCompiledSchema(stringSchema) { fun =>
      assertEquals(doApply(fun, parseJsonValue(""""hello"""")), BasicOutput(true, Seq()))
      assertEquals(doApply(fun, parseJsonValue("13")), BasicOutput(false, Seq(WithPointer(TypeMismatch("string")))))
    }
  }

  test("number") {
    withCompiledSchema(numberSchema) { fun =>
      assertEquals(doApply(fun, parseJsonValue("13")), BasicOutput(true, Seq()))
      assertEquals(doApply(fun, parseJsonValue("null")), BasicOutput(false, Seq(WithPointer(TypeMismatch("number")))))
    }
  }

  test("array") {
    withCompiledSchema(numberArraySchema) { fun =>
      assertEquals(doApply(fun, parseJsonValue("[13]")), BasicOutput(true, Seq()))
      assertEquals(doApply(fun, parseJsonValue("null")), BasicOutput(false, Seq(WithPointer(TypeMismatch("array")))))
    }
  }

  test("array items") {
    withCompiledSchema(numberArraySchema) { fun =>
      assertEquals(doApply(fun, parseJsonValue("null")), BasicOutput(false, Seq(WithPointer(TypeMismatch("array")))))
      assertEquals(doApply(fun, parseJsonValue("[13]")), BasicOutput(true, Seq()))
      assertEquals(
        doApply(fun, parseJsonValue("[true]")),
        BasicOutput(false, Seq(WithPointer(TypeMismatch("number"), Pointer.empty / 0)))
      )
    }
  }

  test("object") {
    withCompiledSchema(totoObjectSchema) { fun =>
      assertEquals(
        doApply(
          fun,
          parseJsonValue("""{
                           |"toto": 13,
                           |"titi": "hello"
                           |}
                           |""".stripMargin)
        ),
        BasicOutput(true, Seq())
      )
      assertEquals(
        doApply(fun, parseJsonValue("null")),
        BasicOutput(false, Seq(WithPointer(TypeMismatch("object"))))
      )
    }
  }

  test("object with pointer") {
    withCompiledSchema(totoObjectSchema) { fun =>
      assertEquals(
        doApply(fun, parseJsonValue("""{"toto": 13,"titi": true}""")),
        BasicOutput(false, Seq(WithPointer(TypeMismatch("string"), Pointer.empty / "titi")))
      )
    }
  }

  test("object missing property") {
    withCompiledSchema(totoObjectSchema) { fun =>
      assertEquals(
        doApply(fun, parseJsonValue("""{"toto": 13}""")),
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
        doApply(fun, parseJsonValue("""{"toto": 13}""")),
        BasicOutput(false, Seq(WithPointer(MissingRequiredProperties(Seq("titi")))))
      )
    }
  }

  test("all of") {
    withCompiledSchema(allOfSchema) { fun =>
      assertEquals(
        doApply(fun, parseJsonValue("13")),
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
        doApply(fun, parseJsonValue("1313")),
        BasicOutput(false, Seq(WithPointer(TypeMismatch("string"))))
      )
    }
  }

  test("any of") {
    withCompiledSchema(anyOfSchema) { fun =>
      assertEquals(
        doApply(fun, parseJsonValue("1313")),
        BasicOutput(true, Seq())
      )
      assertEquals(
        doApply(fun, parseJsonValue("true")),
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
        doApply(fun, parseJsonValue("1313")),
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
        doApply(fun, parseJsonValue("1313")),
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
        doApply(fun, parseJsonValue("1313")),
        BasicOutput(false, Seq(WithPointer(NotOneOf(2))))
      )
    }
  }

  test("not") {
    withCompiledSchema("""{"not": { "type": "number" }}""") { fun =>
      assertEquals(
        doApply(fun, parseJsonValue("true")),
        BasicOutput(true, Seq())
      )
      assertEquals(
        doApply(fun, parseJsonValue("1313")),
        BasicOutput(false, Seq(WithPointer(NotInvalid())))
      )
    }
  }

  test("if/then/else") {
    withCompiledSchema(ifThenElseSchema) { fun =>
      assertEquals(
        doApply(fun, parseJsonValue("1313")),
        BasicOutput(true, Seq())
      )
      assertEquals(
        doApply(fun, parseJsonValue(""""string"""")),
        BasicOutput(true, Seq())
      )
      assertEquals(
        doApply(fun, parseJsonValue("null")),
        BasicOutput(false, Seq(WithPointer(TypeMismatch("string"))))
      )
    }
  }

  test("if/else") {
    withCompiledSchema("""{
                         |"if": { "type": "number" },
                         |"else": { "type": "string" }
                         |}
                         |""".stripMargin) { fun =>
      assertEquals(
        doApply(fun, parseJsonValue("1313")),
        BasicOutput(true, Seq())
      )
      assertEquals(
        doApply(fun, parseJsonValue(""""string"""")),
        BasicOutput(true, Seq())
      )
      assertEquals(
        doApply(fun, parseJsonValue("null")),
        BasicOutput(false, Seq(WithPointer(TypeMismatch("string"))))
      )
    }
  }

  test("if/then") {
    withCompiledSchema("""{
                         |"if": { "type": "number" },
                         |"then": { "type": "number" }
                         |}
                         |""".stripMargin) { fun =>
      assertEquals(
        doApply(fun, parseJsonValue("1313")),
        BasicOutput(true, Seq())
      )
      assertEquals(
        doApply(fun, parseJsonValue(""""string"""")),
        BasicOutput(true, Seq())
      )
      assertEquals(
        doApply(fun, parseJsonValue("null")),
        BasicOutput(true, Seq())
      )
    }
  }

  test("then/else") {
    withCompiledSchema("""{
                         |"then": { "type": "number" },
                         |"else": { "type": "string" }
                         |}
                         |""".stripMargin) { fun =>
      assertEquals(
        doApply(fun, parseJsonValue("1313")),
        BasicOutput(true, Seq())
      )
      assertEquals(
        doApply(fun, parseJsonValue(""""string"""")),
        BasicOutput(true, Seq())
      )
      assertEquals(
        doApply(fun, parseJsonValue("null")),
        BasicOutput(true, Seq())
      )
    }
  }

  test("null or string") {
    withCompiledSchema(nullOrStringSchema) { fun =>
      assertEquals(
        doApply(fun, parseJsonValue("null")),
        BasicOutput(true, Seq())
      )
      assertEquals(
        doApply(fun, parseJsonValue(""""hello"""")),
        BasicOutput(true, Seq())
      )
      assertEquals(
        doApply(fun, parseJsonValue("13")),
        BasicOutput(false, Seq(WithPointer(TypeMismatch("null")), WithPointer(TypeMismatch("string"))))
      )
    }
  }

  test("enum") {
    withCompiledSchema(enumSchema) { fun =>
      assertEquals(
        doApply(fun, parseJsonValue(""""foo"""")),
        BasicOutput(true, Seq())
      )
      assertEquals(
        doApply(fun, parseJsonValue(""""bar"""")),
        BasicOutput(true, Seq())
      )
      assertEquals(
        doApply(fun, parseJsonValue(""""hello"""")),
        BasicOutput(
          false,
          Seq(
            WithPointer(NotInEnum(Seq(StringValue("foo"), StringValue("bar"))))
          )
        )
      )
    }
  }

  test("const") {
    withCompiledSchema(constSchema) { fun =>
      assertEquals(
        doApply(fun, parseJsonValue(""""first"""")),
        BasicOutput(true, Seq())
      )
      assertEquals(
        doApply(fun, parseJsonValue("{}")),
        BasicOutput(
          false,
          Seq(
            WithPointer(NotInEnum(Seq(StringValue("first")))),
            WithPointer(TypeMismatch("string"))
          )
        )
      )
      assertEquals(
        doApply(fun, parseJsonValue(""""second"""")),
        BasicOutput(
          false,
          Seq(
            WithPointer(NotInEnum(Seq(StringValue("first"))))
          )
        )
      )
    }
  }

  // TODO
  test("missing $id/$ref/$def".ignore) {
    withCompiledSchema(missingIdRefDefsSchema) { fun =>
      assertEquals(
        doApply(fun, parseJsonValue("[1313]")),
        BasicOutput(false, Seq(WithPointer(CannotResolve("#missing", None), Pointer.empty / 0)))
      )
    }
  }

  test("$id/$ref/$def") {
    withCompiledSchema(idRefDefsSchema) { fun =>
      assertEquals(
        doApply(fun, parseJsonValue("[1313]")),
        BasicOutput(true, Seq())
      )
      assertEquals(
        doApply(fun, parseJsonValue("""["hello"]""")),
        BasicOutput(false, Seq(WithPointer(TypeMismatch("number"), Pointer.empty / 0)))
      )
    }
  }

  test("$id/$ref/$def bulk") {
    withCompiledSchema(idRefDefsSchema) { fun =>
      assertEquals(
        doApplyBulk(
          fun,
          Seq(parseJsonValue("[1313]"), parseJsonValue("""["hello"]""")),
          state =>
            assertEquals(state.resolved.keySet, Set("#item"))
            assertEquals(state.hits, Map("#item" -> 1))
        ),
        Seq(BasicOutput(true, Seq()), BasicOutput(false, Seq(WithPointer(TypeMismatch("number"), Pointer.empty / 0))))
      )
    }
  }

  test("$ref in properties") {
    withCompiledSchema(refInPropertiesSchema) { fun =>
      assertEquals(
        doApply(fun, parseJsonValue("""{ "foo": 13 }""")),
        BasicOutput(true, Seq())
      )
      assertEquals(
        doApply(fun, parseJsonValue("""{ "foo": true }""")),
        BasicOutput(false, Seq(WithPointer(TypeMismatch("number"), Pointer.empty / "foo")))
      )
    }
  }

  test("$ref at root") {
    withCompiledSchema(refAtRootSchema) { fun =>
      assertEquals(
        doApplyBulk(
          fun,
          Seq(
            parseJsonValue("""{ "foo": 13 }"""),
            parseJsonValue("""{ "foo": [13] }"""),
            parseJsonValue("""{ "foo": true }""")
          ),
          state =>
            assertEquals(state.resolved.keySet, Set("#object", "#/$defs/numberType"))
            assertEquals(state.hits, Map("#/$defs/numberType" -> 5, "#object" -> 2))
        ),
        Seq(
          BasicOutput(true, Seq()),
          BasicOutput(true, Seq()),
          BasicOutput(
            false,
            Seq(
              WithPointer(TypeMismatch("number"), Pointer.empty / "foo"),
              WithPointer(TypeMismatch("array"), Pointer.empty / "foo")
            )
          )
        )
      )
    }
  }

  test("$ref to validation spec, with two '$ref's".only) {
    val lazyResolver = Some(MetaSchemas.lazyResolver)
    withCompiledSchema(refToValidationSpec, lazyResolver) { fun =>
      assertEquals(
        doApplyBulk(
          fun,
          Seq(
            parseJsonValue("""{ "$defs": { "foo": { "type": "boolean" } } }"""),
            parseJsonValue("""{ "$defs": { "foo": { "type": ["boolean"] } } }"""),
            parseJsonValue("""{ "$defs": { "foo": { "type": 13 } } }""")
          ),
          state =>
            assertEquals(
              state.resolved.keySet,
              Set(
                //                "https://json-schema.org/draft/2020-12/meta/validation",
                //                "https://json-schema.org/draft/2020-12/meta/core"
              )
            )
            assertEquals(
              state.hits,
              Map(
                //                "https://json-schema.org/draft/2020-12/meta/validation" -> 2,
                //                "https://json-schema.org/draft/2020-12/meta/core"       -> 2
              )
            )
        ),
        Seq(
          BasicOutput(true, Seq()),
          BasicOutput(true, Seq()),
          BasicOutput(
            false,
            Seq(
              WithPointer(
                NotInEnum(
                  values = Seq(
                    "array",
                    "boolean",
                    "integer",
                    "null",
                    "number",
                    "object",
                    "string"
                  ).map(StringValue.apply)
                ),
                pointer = Pointer.parse("/$defs/foo/type")
              ),
              WithPointer(
                TypeMismatch(
                  expected = "array"
                ),
                pointer = Pointer.parse("/$defs/foo/type")
              )
            )
          )
        )
      )
    }
  }

object Util:
  private val vocabularyForTest = dialect(Seq(Vocabulary.coreId, Vocabulary.validationId, Vocabulary.applicatorId))

  def withKeywords(schema: SchemaValue, lazyResolver: Option[LoadedSchemasResolver.LazyResolver] = None)(
    f: Keywords => Unit
  ): Unit =
  // TODO avoid lazyResolver
    Keywords(schema, vocabularyForTest, lazyResolver)
      .fold(
        errors => throw new IllegalArgumentException(s"no keywords: $errors"),
        keywords => f(keywords)
      )

  type AssertingFun[R[_], O] = SchemaResolver ?=> R[Value => O] => Unit

  def withCompiledSchema[R[_], O](
                                   schema: String,
                                   lazyResolver: Option[LoadedSchemasResolver.LazyResolver] = None
                                 )(using eval: Eval[R, O])(using TheResultMonad[R, O])(f: AssertingFun[R, O]): Unit =
    withSchema(schema) { schema =>
      withKeywords(schema, lazyResolver) { keywords =>
        val fun = eval.fun(eval.compile(keywords))

        given SchemaResolver = LoadedSchemasResolver(schema, lazyResolver)

        f(fun)
      }
    }

  // TODO possible?
  // case class MyState[O: OutputOps](resolver: SchemaResolver, count: Int, resolved: Map[String, MyR[Eval.Fun[O]]])
  case class MyState(
      resolver: SchemaResolver,
      count: Int,
      resolved: Map[String, Either[SchemaProblems, Keywords]],
      hits: Map[String, Int]
  )
  type MyR[O] = [A] =>> MyState => (A, MyState)
  def myZero[O: OutputOps](resolver: SchemaResolver) = MyState(resolver, 0, Map.empty, Map.empty)

  given [O: OutputOps]: TheResultMonad[MyR[O], O] with
    def unit[A](a: A): MyR[O][A] = s => (a, s)

    def bind[A, B](a: MyR[O][A])(f: A => MyR[O][B]): MyR[O][B] = s =>
      val (a2, s2) = a(s)
      f(a2)(s2.copy(count = s2.count + 1))

    def resolve(resolution: SchemaResolution, vocabulary: Vocabulary, scope: DynamicScope)(using
                                                                                           eval: Eval[MyR[O], O]
    ): MyR[O][Eval.Fun[O]] =
      MyState.resolve(resolution, vocabulary, scope)

    def resolveDynamic(resolution: SchemaResolution, vocabulary: Vocabulary, scope: DynamicScope)(using
                                                                                                  eval: Eval[MyR[O], O]
    ): MyR[O][Eval.Fun[O]] =
      MyState.resolveDynamic(resolution, vocabulary, scope)

  object MyState:
    def resolve[O: OutputOps](resolution: SchemaResolution, vocabulary: Vocabulary, scope: DynamicScope)(using
                                                                                                         eval: Eval[MyR[O], O]
    ): MyR[O][Eval.Fun[O]] =
      val ops = summon[OutputOps[O]]
      (state: MyState) =>
        //        val alreadyResolved = state.resolved
        //          .get("maybelater")
        //          .map { ks =>
        ////            println(s"FW already resolved ${ref} ${ks.map(_.keywords.size)}")
        //            val state1 = state.copy(hits = state.hits.updatedWith(ref)(_.map(_ + 1).orElse(Some(1))))
        //            (ks, state1)
        //          }
        //        lazy val newlyResolved = state.resolver
        //          .resolveRef(ref)
        //          .map(resolution =>
        //            // TODO dynamic scope
        //            // TODO vocabulary
        //            val ks = Keywords.parseKeywords(vocabulary, resolution, scope)
        ////            println(
        ////              s"FW newly resolved ${resolution.resolver.base} ${ks.map(_.keywords.size)}"
        ////            )
        //            val state1 = state.copy(resolved = state.resolved + (ref -> ks))
        //            (ks, state1)
        //          )
        val resolved = Some((Keywords.parseKeywords(vocabulary, resolution, scope), state))
        val compiled = resolved
          //          .orElse(newlyResolved)
          .map { (ks, state) =>
            ks.fold(
              problems => {
                val ref = resolution.resolver.base.toString
                val f =
                  (value: WithPointer[Value]) => ops.invalid(CannotResolve(ref, Some(problems)), value.pointer)
                (f, state)
              },
              ks =>
                val f = (value: WithPointer[Value]) =>
                  //                  println(s"FW late compiled ${ks.schema}")
                  // TODO lost state update?
                  val (compiled, state1) = eval.compile(ks)(state)
                  compiled(value)
                (f, state)
            )
          }
        compiled.getOrElse {
          // TODO
          // val f = (value: WithPointer[Value]) => ops.invalid(MissingReference(ref), value.pointer)
          val ref = resolution.resolver.base.toString
          val f = (value: WithPointer[Value]) => ops.invalid(CannotResolve(ref, None), value.pointer)
          (f, state)
        }

    def resolveDynamic[O: OutputOps](resolution: SchemaResolution, vocabulary: Vocabulary, scope: DynamicScope)(using
                                                                                                                eval: Eval[MyR[O], O]
    ): MyR[O][Eval.Fun[O]] =
      val ops = summon[OutputOps[O]]
      (state: MyState) =>
        //        state.resolver
        //          .resolveDynamicRef(ref, scope.parent())
        //          .map(resolution =>
        //            println(s"FW dynamically resolved ${ref} ${resolution.resolver.base}")
        //            // TODO dynamic scope?
        //            // TODO vocabulary
        //            val ks = Keywords.parseKeywords(vocabularyForTest.get, resolution, scope)
        //            // val state1 = state.copy(resolved = state.resolved + (ref -> ks))
        //            // TODO cache dynamic resolutions??
        //            (ks, state)
        //          )
        val parsed = Some((Keywords.parseKeywords(vocabulary, resolution, scope), state))
        parsed
          .map { (ks, state) =>
            ks.fold(
              problems => {
                val ref = resolution.resolver.base.toString
                val f =
                  (value: WithPointer[Value]) =>
                    ops.invalid(CannotResolveDynamic(ref, scope, Some(problems)), value.pointer)
                (f, state)
              },
              ks =>
                val f = (value: WithPointer[Value]) =>
                  //                  println(s"FW late compiled ${ks.schema}")
                  // TODO lost state update?
                  val (compiled, state1) = eval.compile(ks)(state)
                  compiled(value)
                (f, state)
            )
          }
          .getOrElse {
            // TODO
            // val f = (value: WithPointer[Value]) => ops.invalid(MissingReference(ref), value.pointer)
            val ref = resolution.resolver.base.toString
            val f = (value: WithPointer[Value]) => ops.invalid(CannotResolveDynamic(ref, scope, None), value.pointer)
            (f, state)
          }
// for
//   resolution <- resolver
//     .resolveDynamicRef(ref, scope)
//     .map(Right(_))
//     .getOrElse(Left(SchemaProblems(MissingDynamicReference(ref))))
//   vocabulary1 <- SchemaValue.vocabulary(resolution, vocabulary)
//   keyword = lazyResolve(vocabulary1, resolution, scope1)
// yield add(keyword).add(DynamicRefKeyword(ref, scope))
