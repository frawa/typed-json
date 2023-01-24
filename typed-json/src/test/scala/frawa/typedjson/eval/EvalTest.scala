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

  val eval             = Eval[MyR, MyO]
  given Eval[MyR, MyO] = eval

  test("null") {
    withCompiledSchema(nullSchema) { fun =>
      assertEquals(fun(NullValue), MyO(true))
      assertEquals(fun(BoolValue(true)), MyO(false))
    }
  }

  test("true") {
    withCompiledSchema(trueSchema) { fun =>
      assertEquals(fun(BoolValue(true)), MyO(true))
      assertEquals(fun(NullValue), MyO(true))
    }
  }

object Util:
  private val vocabularyForTest = dialect(Seq(Vocabulary.coreId, Vocabulary.validationId, Vocabulary.applicatorId))

  def withKeywords(schema: SchemaValue)(f: Keywords => Unit): Unit =
    Keywords(schema, vocabularyForTest, None)
      .map { keywords =>
        f(keywords)
      }

  def withCompiledSchema[R[_], O](schema: String)(using eval: Eval[R, O])(f: R[Eval.Fun[O]] => Unit): Unit =
    withSchema(schema) { schema =>
      withKeywords(schema) { keywords =>
        val fun = eval.compile(keywords)
        f(fun)
      }
    }

  type MyR[O] = O
  case class MyO(valid: Boolean)

  given TheResultMonad[MyR] with
    def unit[A](a: A): MyR[A]                            = a
    def flatMap[A, B](a: MyR[A])(f: A => MyR[B]): MyR[B] = f(a)
    def output[O](result: MyR[O]): O                     = result

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
      def not: MyO         = ???
      def isValid: Boolean = ???
    //   def combine(o2: MyO): MyO = all(Seq(o, o2))
