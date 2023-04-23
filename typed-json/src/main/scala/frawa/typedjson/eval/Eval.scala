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

import frawa.typedjson.keywords.*
import frawa.typedjson.parser.Value
import frawa.typedjson.parser.Value.*
import frawa.typedjson.pointer.Pointer
import frawa.typedjson.validation.{TypeMismatch, ValidationAnnotation, ValidationError}

import java.net.URI
import scala.reflect.TypeTest

trait TheResultMonad[R[_], O: OutputOps] extends FP.Monad[R]:
  def unit[A](a: A): R[A]

  def bind[A, B](a: R[A])(f: A => R[B]): R[B]

  // extras
  def resolve(ref: String, base: URI, scope: DynamicScope)(using
      eval: Eval[R, O]
  ): Eval.Fun[R[O]]

  def resolveDynamic(ref: String, base: URI, scope: DynamicScope)(using
      eval: Eval[R, O]
  ): Eval.Fun[R[O]]
  //

  extension [A](r: R[A])
    def flatMap[B](f: A => R[B]): R[B] = bind(r)(f)
    def map[B](f: A => B): R[B]        = bind(r)(a => unit(f(a)))
end TheResultMonad

object Eval:
  type Fun[O] = WithPointer[Value] => O
  def map[O](fun: Fun[O])(f: O => O): Fun[O] = value => f(fun(value))

class Eval[R[_], O](using TheResultMonad[R, O], OutputOps[O]):

  import Eval.Fun
  import Keywords.KeywordWithLocation

  private val monad = summon[TheResultMonad[R, O]]

  private def verify = Verify[R, O]

  final def fun(compiled: Fun[R[O]]): Value => R[O] = value => compiled(WithPointer(value))

  final def compile(keywords: Keywords): Fun[R[O]] =
    val ks  = keywords.keywords.toSeq
    val fun = compileSeqWith(ks)
    verify.verifyAllOf(fun)

  private final def compile(keyword: Keyword): Fun[R[O]] =
    compileOne(keyword)

  private final def compileSeqWith(kws: Seq[KeywordWithLocation]): Fun[R[Seq[O]]] =
    val ks = kws.map(_.value)
    compileSeq(ks)

  private final def compileSeq(ks: Seq[Keyword]): Fun[R[Seq[O]]] =
    val funs = ks.map(compile)
    funSequence(funs)

  private final def funSequence(funs: Seq[Fun[R[O]]]): Fun[R[Seq[O]]] =
    funs.foldLeft(funUnit(Seq.empty[O])) { (acc, fun) =>
      funFlatMap(fun)(o => value => acc(value).map(os => os :+ o))
    }

  private final def funUnit[A](a: A): Fun[R[A]] =
    value => monad.unit(a)

  private final def funFlatMap[A, B](fun: Fun[R[A]])(f: A => Fun[R[B]]): Fun[R[B]] =
    value => fun(value).flatMap(a => f(a)(value))

  private final def compileSeqKeywords(kks: Seq[Keywords]): Fun[R[Seq[O]]] =
    val funs = kks.map(compile)
    funSequence(funs)

  private final def compile(kks: Map[String, Keywords]): Map[String, Fun[R[O]]] =
    kks.view.mapValues(compile).toMap

  private def unitFun(f: Fun[O]): Fun[R[O]] =
    value => monad.unit(f(value))

  private def compileOne(k: Keyword): Fun[R[O]] =
    k match {
      case NullTypeKeyword      => unitFun(verify.verifyType(verify.nullTypeMismatch))
      case TrivialKeyword(v)    => unitFun(verify.verifyTrivial(v))
      case BooleanTypeKeyword   => unitFun(verify.verifyType(verify.booleanTypeMismatch))
      case NumberTypeKeyword    => unitFun(verify.verifyType(verify.numberTypeMismatch))
      case IntegerTypeKeyword   => unitFun(verify.verifyInteger())
      case StringTypeKeyword    => unitFun(verify.verifyType(verify.stringTypeMismatch))
      case ArrayTypeKeyword     => unitFun(verify.verifyType(verify.arrayTypeMismatch))
      case ObjectTypeKeyword    => unitFun(verify.verifyType(verify.objectTypeMismatch))
      case UnionTypeKeyword(ks) => verify.verifyUnion(compileSeqWith(ks))
      case NotKeyword(kk)       => verify.verifyNot(compile(kk))
      case ArrayItemsKeyword(items, prefixItems) =>
        val funItems       = items.map(compile)
        val funPrefixItems = prefixItems.map(compile)
        verify.verifyArrayItems(funItems, funPrefixItems)
      case ObjectPropertiesKeyword(properties, patternProperties, additionalProperties) =>
        val funProperties           = compile(properties)
        val funPatternProperties    = compile(patternProperties)
        val funAdditionalProperties = additionalProperties.map(compile)
        verify.verfyObjectProperties(funProperties, funPatternProperties, funAdditionalProperties)
      case ObjectRequiredKeyword(names) => unitFun(verify.verifyObjectRequired(names))
      case AllOfKeyword(kks)            => verify.verifyAllOf(compileSeqKeywords(kks))
      case AnyOfKeyword(kks)            => verify.verifyAnyOf(compileSeqKeywords(kks))
      case OneOfKeyword(kks)            => verify.verifyOneOf(compileSeqKeywords(kks))
      case IfThenElseKeyword(ksIf, ksThen, ksElse) =>
        val funIf   = ksIf.map(compile)
        val funThen = ksThen.map(compile)
        val funElse = ksElse.map(compile)
        verify.verfyIfThenElse(funIf, funThen, funElse)
      case EnumKeyword(vs) => unitFun(verify.verifyEnum(vs))
      case RefKeyword(ref, base, scope) =>
        given Eval[R, O] = this
        monad.resolve(ref, base, scope)
      case DynamicRefKeyword(ref, base, scope) =>
        given Eval[R, O] = this
        monad.resolveDynamic(ref, base, scope)
      case MinItemsKeyword(min)         => unitFun(verify.verifyMinItems(min))
      case UniqueItemsKeyword(unique)   => unitFun(verify.verifyUniqueItems(unique))
      case MinimumKeyword(min, exclude) => unitFun(verify.verifyMinimum(min, exclude))
      case PatternKeyword(pattern)      => unitFun(verify.verifyPattern(pattern))
      case PropertyNamesKeyword(ks)     => verify.verifyPropertyNames(compile(ks))
      case FormatKeyword(format)        => unitFun(verify.verifyFormat(format))
      // ...
      // TODO to be removed, ignore for now
      // case _: LazyParseKeywords => unitFun(value => summon[OutputOps[O]].valid(value.pointer))
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

trait ResultOps[R[_]] extends FP.Monad[R]
