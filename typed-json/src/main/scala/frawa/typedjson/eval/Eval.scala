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

import frawa.typedjson.keywords.Keywords
import frawa.typedjson.keywords.InnerValue
import frawa.typedjson.keywords.Keyword
import frawa.typedjson.pointer.Pointer
import frawa.typedjson.validation.ValidationError
import frawa.typedjson.validation.ValidationAnnotation
import frawa.typedjson.keywords.SchemaProblems
import frawa.typedjson.keywords.BooleanTypeKeyword
import frawa.typedjson.validation.TypeMismatch
import frawa.typedjson.parser.Value
import frawa.typedjson.parser.Value._
import frawa.typedjson.keywords.NullTypeKeyword
import frawa.typedjson.keywords.NotKeyword
import frawa.typedjson.keywords.UnionTypeKeyword
import scala.reflect.TypeTest
import frawa.typedjson.keywords.TrivialKeyword
import frawa.typedjson.keywords.ArrayItemsKeyword
import frawa.typedjson.keywords.NumberTypeKeyword
import frawa.typedjson.keywords.ArrayTypeKeyword
import frawa.typedjson.keywords.ObjectPropertiesKeyword
import frawa.typedjson.keywords.StringTypeKeyword
import frawa.typedjson.keywords.ObjectTypeKeyword
import frawa.typedjson.keywords.WithPointer
import frawa.typedjson.keywords.ObjectRequiredKeyword
import frawa.typedjson.keywords.AllOfKeyword
import frawa.typedjson.keywords.AnyOfKeyword
import frawa.typedjson.keywords.OneOfKeyword
import frawa.typedjson.keywords.IfThenElseKeyword
import frawa.typedjson.keywords.EnumKeyword

trait TheResultMonad[R[_]]:
  def unit[A](a: A): R[A]
  def bind[A, B](a: R[A])(f: A => R[B]): R[B]
  def fetch[O](result: R[O]): O
  extension [A](r: R[A])
    def flatMap[B](f: A => R[B]): R[B] = bind(r)(f)
    def map[B](f: A => B): R[B]        = bind(r)(a => unit(f(a)))
    def get: A                         = fetch(r)
end TheResultMonad

object Eval:
  type Fun[O] = WithPointer[Value] => O
  def map[O](fun: Fun[O])(f: O => O): Fun[O] = value => f(fun(value))

class Eval[R[_]: TheResultMonad, O: OutputOps]:
  import Eval.Fun
  import Keywords.KeywordWithLocation

  protected val monad  = summon[TheResultMonad[R]]
  protected val verify = Verify[O]

  final def fun(compiled: R[Fun[O]]): R[Value => O] = monad.map(compiled)(f => value => f(WithPointer(value)))

  final def compile(keywords: Keywords): R[Fun[O]] =
    monad.map(compile(keywords.keywords.toSeq))(fs => verify.verifyAll(fs))

  private final def compile(keyword: Keyword): R[Fun[O]] =
    compileOne(keyword)

  private final def compile(o: Option[Keywords]): R[Option[Fun[O]]] =
    o.map(o => compile(o)).map(o => monad.map(o)(o => Option(o))).getOrElse(monad.unit(Option.empty))

  private final def compile(keywords: Seq[KeywordWithLocation]): R[Seq[Fun[O]]] =
    val fs = keywords.map(_.value).map(compile)
    fs.foldLeft(monad.unit(Seq())) { (acc, f) =>
      for {
        acc <- acc
        f   <- f
      } yield {
        acc :+ f
      }
    }

  private final def compile2(ks: Seq[Keywords]): R[Seq[Fun[O]]] =
    val fs = ks.map(compile)
    fs.foldLeft(monad.unit(Seq())) { (acc, f) =>
      for {
        acc <- acc
        f   <- f
      } yield {
        acc :+ f
      }
    }

  private final def compile(o: Map[String, Keywords]): R[Map[String, Fun[O]]] =
    val fs = o.view.mapValues(o => compile(o))
    fs.foldLeft(monad.unit(Map.empty[String, Fun[O]])) { case (acc, (p, f)) =>
      for {
        acc <- acc
        f   <- f
      } yield {
        acc + (p -> f)
      }
    }

  private def compileOne(k: Keyword): R[Fun[O]] =
    k match {
      case NullTypeKeyword      => monad.unit(verify.verifyType(verify.nullTypeMismatch))
      case TrivialKeyword(v)    => monad.unit(verify.verifyTrivial(v))
      case BooleanTypeKeyword   => monad.unit(verify.verifyType(verify.booleanTypeMismatch))
      case NumberTypeKeyword    => monad.unit(verify.verifyType(verify.numberTypeMismatch))
      case StringTypeKeyword    => monad.unit(verify.verifyType(verify.stringTypeMismatch))
      case ArrayTypeKeyword     => monad.unit(verify.verifyType(verify.arrayTypeMismatch))
      case ObjectTypeKeyword    => monad.unit(verify.verifyType(verify.objectTypeMismatch))
      case UnionTypeKeyword(ks) => monad.map(compile(ks))(fs => verify.verifyUnion(fs))
      case NotKeyword(ks)       => compile(ks).map(f => verify.verifyNot(f))
      case ArrayItemsKeyword(items, prefixItems) =>
        for {
          items       <- compile(items)
          prefixItems <- compile2(prefixItems)
        } yield {
          verify.verfyArrayItems(items, prefixItems)
        }
      case ObjectPropertiesKeyword(properties, patternProperties, additionalProperties) =>
        for {
          properties           <- compile(properties)
          patternProperties    <- compile(patternProperties)
          additionalProperties <- compile(additionalProperties)
        } yield {
          verify.verfyObjectProperties(properties, patternProperties, additionalProperties)
        }
      case ObjectRequiredKeyword(names) => monad.unit(verify.verifyObjectRequired(names))
      case AllOfKeyword(ks)             => compile2(ks).map(f => verify.verifyAllOf(f))
      case AnyOfKeyword(ks)             => compile2(ks).map(f => verify.verifyAnyOf(f))
      case OneOfKeyword(ks)             => compile2(ks).map(f => verify.verifyOneOf(f))
      case IfThenElseKeyword(ksIf, ksThen, ksElse) =>
        for {
          ksIf   <- compile(ksIf)
          ksThen <- compile(ksThen)
          ksElse <- compile(ksElse)
        } yield {
          verify.verfyIfThenElse(ksIf, ksThen, ksElse)
        }
      case EnumKeyword(vs) => monad.unit(verify.verifyEnum(vs))
      // ...
    }

trait OutputOps[O]: // extends Monoid[O]:
  def valid(pointer: Pointer): O
  // def valid(annotation: ValidationAnnotation, pointer: Pointer): O
  def invalid(error: ValidationError, pointer: Pointer): O
  // def invalid(problems: SchemaProblems): O

  def all(os: Seq[O], pointer: Pointer): O
  // def contains(os: Seq[O], min: Option[Int], max: Option[Int], pointer: Pointer): O

  // def unit = valid
  extension (o: O)
    def not(pointer: Pointer): O
    def isValid: Boolean
    // def combine(o2: O): O = all(Seq(o, o2))

trait ResultOps[R[_]] extends Monad[R]

object Util:
  def sequence[A, F[A]](cs: Seq[F[A]])(using monad: Monad[F]): F[Seq[A]] =
    cs.foldLeft(monad.pure(Seq.empty)) { (acc, c) =>
      acc.flatMap(cs => c.map(cs :+ _))
    }

trait State[S]: // extends Monad[[A] =>> State[A] => (A, State[A])]:
  type S1[A] = S => (A, S)

  def pure[A](a: A): S1[A] = s => (a, s)
  def flatMap[A, B](a: S1[A])(f: A => S1[B]): S1[B] = s0 =>
    val (aa, s1) = a(s0)
    f(aa)(s1)

trait Monad[F[_]] extends Functor[F]:

  /** The unit value for a monad */
  def pure[A](x: A): F[A]

  extension [A](x: F[A])
    /** The fundamental composition operation */
    def flatMap[B](f: A => F[B]): F[B]

  /** The `map` operation can now be defined in terms of `flatMap` */
  // def map[B](f: A => B) = x.flatMap(f.andThen(pure))

trait Functor[F[_]]:
  extension [A](x: F[A]) def map[B](f: A => B): F[B]

trait SemiGroup[T]:
  extension (x: T) def combine(y: T): T

trait Monoid[T] extends SemiGroup[T]:
  def unit: T
