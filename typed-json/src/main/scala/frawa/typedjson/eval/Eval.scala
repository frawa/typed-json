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
import frawa.typedjson.output.OutputOps
import frawa.typedjson.util.FP
import frawa.typedjson.util.WithPointer

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
  type EvalFun[R[_], O] = Value => R[O]
  type Fun[O]           = WithPointer[Value] => O
  def map[O](fun: Fun[O])(f: O => O): Fun[O] = value => f(fun(value))

class Eval[R[_], O](using TheResultMonad[R, O], OutputOps[O]):

  import Eval.EvalFun
  import Eval.Fun

  private val monad = summon[TheResultMonad[R, O]]

  private def verify = Verify[R, O]

  final def fun(compiled: Fun[R[O]]): EvalFun[R, O] = value => compiled(WithPointer(value))

  final def compile(keywords: Keywords, kl: KeywordLocation): Fun[R[O]] =
    val ks  = keywords.keywords.toSeq
    val fun = compileSeq(ks, kl)
    value => verify.verifyAllOf(fun)(value).map(_.forKeyword(AllOfKeyword(Seq()), kl))

  private final def compileSeq(ks: Seq[Keyword], kl: KeywordLocation): Fun[R[Seq[O]]] =
    val funs = ks.map(compileOne(_, kl))
    funSequence(funs)

  private final def funSequence(funs: Seq[Fun[R[O]]]): Fun[R[Seq[O]]] =
    value =>
      val ros = funs.map(fun => fun(value))
      FP.sequence(ros)

  private final def compileSeqKeywords(kks: Seq[Keywords], kl: KeywordLocation): Fun[R[Seq[O]]] =
    val funs = kks.map(compile(_, kl))
    funSequence(funs)

  private final def compile(kks: Map[String, Keywords], kl: KeywordLocation): Map[String, Fun[R[O]]] =
    kks.view.mapValues(compile(_, kl)).toMap

  private def compileOne(k: Keyword, kl: KeywordLocation): Fun[R[O]] =
    val fun = k match {
      case NullTypeKeyword      => verify.verifyType(verify.nullTypeMismatch)
      case TrivialKeyword(v)    => verify.verifyTrivial(v)
      case BooleanTypeKeyword   => verify.verifyType(verify.booleanTypeMismatch)
      case NumberTypeKeyword    => verify.verifyType(verify.numberTypeMismatch)
      case IntegerTypeKeyword   => verify.verifyInteger()
      case StringTypeKeyword    => verify.verifyType(verify.stringTypeMismatch)
      case ArrayTypeKeyword     => verify.verifyType(verify.arrayTypeMismatch)
      case ObjectTypeKeyword    => verify.verifyType(verify.objectTypeMismatch)
      case UnionTypeKeyword(ks) => verify.verifyUnion(compileSeq(ks, kl))
      case NotKeyword(kk)       => verify.verifyNot(compile(kk, kl))
      case ArrayItemsKeyword(items, prefixItems) =>
        val funItems       = items.map(compile(_, kl))
        val funPrefixItems = prefixItems.map(compile(_, kl))
        verify.verifyArrayItems(funItems, funPrefixItems)
      case ObjectPropertiesKeyword(properties, patternProperties, additionalProperties) =>
        val funProperties        = compile(properties, kl)
        val funPatternProperties = compile(patternProperties, kl)
        val funAdditionalProperties = additionalProperties.map(additionalProperties =>
          verify.verifyAdditionalProperties((compile(additionalProperties, kl)))
        )
        verify.verfyObjectProperties(funProperties, funPatternProperties, funAdditionalProperties)
      case ObjectRequiredKeyword(names) => verify.verifyObjectRequired(names)
      case AllOfKeyword(kks)            => verify.verifyAllOf(compileSeqKeywords(kks, kl))
      case AnyOfKeyword(kks)            => verify.verifyAnyOf(compileSeqKeywords(kks, kl))
      case OneOfKeyword(kks)            => verify.verifyOneOf(compileSeqKeywords(kks, kl))
      case IfThenElseKeyword(ksIf, ksThen, ksElse) =>
        val funIf   = ksIf.map(compile(_, kl))
        val funThen = ksThen.map(compile(_, kl))
        val funElse = ksElse.map(compile(_, kl))
        verify.verfyIfThenElse(funIf, funThen, funElse)
      case EnumKeyword(vs) => verify.verifyEnum(vs)
      case RefKeyword(ref, base, scope) =>
        given Eval[R, O] = this
        monad.resolve(ref, base, scope)
      case DynamicRefKeyword(ref, base, scope) =>
        given Eval[R, O] = this
        monad.resolveDynamic(ref, base, scope)
      case MinItemsKeyword(min)               => verify.verifyMinItems(min)
      case MaxItemsKeyword(min)               => verify.verifyMaxItems(min)
      case UniqueItemsKeyword(unique)         => verify.verifyUniqueItems(unique)
      case MinimumKeyword(min, exclude)       => verify.verifyMinimum(min, exclude)
      case MaximumKeyword(max, exclude)       => verify.verifyMaximum(max, exclude)
      case PatternKeyword(pattern)            => verify.verifyPattern(pattern)
      case PropertyNamesKeyword(ks)           => verify.verifyPropertyNames(compile(ks, kl))
      case FormatKeyword(format)              => verify.verifyFormat(format)
      case MultipleOfKeyword(n)               => verify.verifyMultiple(n)
      case MaxLengthKeyword(n)                => verify.verifyMaxLength(n)
      case MinLengthKeyword(n)                => verify.verifyMinLength(n)
      case MaxPropertiesKeyword(max)          => verify.verifyMaxProperties(max)
      case MinPropertiesKeyword(max)          => verify.verifyMinProperties(max)
      case DependentRequiredKeyword(required) => verify.verifyDependentRequired(required)
      case DependentSchemasKeyword(keywords)  => verify.verifyDependentSchemas(compile(keywords, kl))
      case ContainsKeyword(schema, min, max)  => verify.verifyContains(schema.map(compile(_, kl)), min, max)
      case UnevaluatedItemsKeyword(pushed, unevaluated) =>
        val funs = pushed.keywords.map(compileOne(_, kl)).toSeq
        verify.verifyUnevaluatedItems(funs, compile(unevaluated, kl))
      case UnevaluatedPropertiesKeyword(pushed, unevaluated) =>
        val funs = pushed.keywords.map(compileOne(_, kl)).toSeq
        verify.verifyUnevaluatedProperties(funs, compile(unevaluated, kl))
      case WithLocation(_, k, kl)  => compileOne(k, kl)
      case IgnoredKeyword(keyword) => verify.verifyIgnored(keyword)
    }
    k match {
      case _: WithLocation      => fun
      case _: RefKeyword        => fun
      case _: DynamicRefKeyword => fun
      case _                    => value => fun(value).map(_.forKeyword(k, kl))
    }
