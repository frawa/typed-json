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

import frawa.typedjson.keywords.SchemaProblems.{
  InvalidSchemaValue,
  MissingDynamicReference,
  MissingReference,
  UnsupportedKeyword
}
import frawa.typedjson.parser.*
import frawa.typedjson.parser.Value.*
import frawa.typedjson.pointer.Pointer
import frawa.typedjson.util.UriUtil
import frawa.typedjson.util.UriUtil.CurrentLocation
import frawa.typedjson.util.EitherUtil.*

import java.net.URI
import scala.reflect.TypeTest

sealed trait Keyword
sealed trait TypeKeyword extends Keyword

case class TrivialKeyword(v: Boolean)                extends Keyword
case object NullTypeKeyword                          extends TypeKeyword
case object BooleanTypeKeyword                       extends TypeKeyword
case object StringTypeKeyword                        extends TypeKeyword
case object NumberTypeKeyword                        extends TypeKeyword
case object IntegerTypeKeyword                       extends TypeKeyword
case object ArrayTypeKeyword                         extends TypeKeyword
case object ObjectTypeKeyword                        extends TypeKeyword
case class ObjectRequiredKeyword(names: Seq[String]) extends Keyword
case class NotKeyword(keywords: Keywords)            extends Keyword
case class AllOfKeyword(keywords: Seq[Keywords])     extends Keyword
case class AnyOfKeyword(keywords: Seq[Keywords])     extends Keyword
case class OneOfKeyword(keywords: Seq[Keywords])     extends Keyword
case class UnionTypeKeyword(keywords: Seq[Keyword])  extends Keyword
case class EnumKeyword(values: Seq[Value])           extends Keyword
case class ArrayItemsKeyword(
    items: Option[Keywords] = None,
    prefixItems: Seq[Keywords] = Seq()
) extends Keyword
case class ObjectPropertiesKeyword(
    properties: Map[String, Keywords] = Map(),
    patternProperties: Map[String, Keywords] = Map(),
    additionalProperties: Option[Keywords] = None
) extends Keyword
case class IfThenElseKeyword(
    ifKeywords: Option[Keywords] = None,
    thenKeywords: Option[Keywords] = None,
    elseKeywords: Option[Keywords] = None
) extends Keyword
case class PatternKeyword(pattern: String)                                extends Keyword
case class FormatKeyword(format: String)                                  extends Keyword
case class MinimumKeyword(min: BigDecimal, exclude: Boolean = false)      extends Keyword
case class UniqueItemsKeyword(unique: Boolean)                            extends Keyword
case class PropertyNamesKeyword(keywords: Keywords)                       extends Keyword
case class RefKeyword(ref: String, base: URI, scope: DynamicScope)        extends Keyword
case class DynamicRefKeyword(ref: String, base: URI, scope: DynamicScope) extends Keyword
case class MultipleOfKeyword(n: BigDecimal)                               extends Keyword
case class MaximumKeyword(max: BigDecimal, exclude: Boolean = false)      extends Keyword
case class MaxLengthKeyword(max: BigDecimal)                              extends Keyword
case class MinLengthKeyword(min: BigDecimal)                              extends Keyword
case class MaxItemsKeyword(max: BigDecimal)                               extends Keyword
case class MinItemsKeyword(min: BigDecimal)                               extends Keyword
case class MaxPropertiesKeyword(max: BigDecimal)                          extends Keyword
case class MinPropertiesKeyword(min: BigDecimal)                          extends Keyword
case class DependentRequiredKeyword(required: Map[String, Seq[String]])   extends Keyword
case class DependentSchemasKeyword(keywords: Map[String, Keywords])       extends Keyword
case class ContainsKeyword(schema: Option[Keywords] = None, min: Option[Int] = None, max: Option[Int] = None)
    extends Keyword
case class UnevaluatedItemsKeyword(pushed: Keywords, unevaluated: Keywords)      extends Keyword
case class UnevaluatedPropertiesKeyword(pushed: Keywords, unevaluated: Keywords) extends Keyword
case class WithLocation(uri: URI, keyword: Keyword)                              extends Keyword
case class IgnoredKeyword(keyword: String)                                       extends Keyword

case class Keywords(
    vocabulary: Vocabulary,
    keywords: Set[Keyword] = Set(),
    lastKeywords: Seq[Keywords => Keywords] = Seq()
):
  import Keywords.*

  def map[E](f: Keyword => E): Seq[E] =
    keywords.map(f).toSeq

  def flatMap[E](f: Keyword => Set[E]): Seq[E] =
    keywords.flatMap(f).toSeq

  private def withLocation(keyword: Keyword)(using location: CurrentLocation): WithLocation =
    WithLocation(location.uri, keyword)

  private def add(keyword: Keyword)(using location: CurrentLocation): Keywords =
    this.copy(keywords = keywords + withLocation(keyword))

  private def addLast(push: Keywords => Keywords)(using location: CurrentLocation): Keywords =
    this.copy(lastKeywords = lastKeywords :+ push)

  private def addAll(schemas: Seq[SchemaValue], resolver: SchemaResolver, scope: DynamicScope)(
      f: Seq[Keywords] => Keyword
  ): Either[SchemaProblems, Keywords] =
    val keywords0 = schemas.zipWithIndex.map { case (v, i) =>
      Keywords.parseKeywords(vocabulary, resolver.push(v), scope.push(i))
    }
    given CurrentLocation = scope.currentLocation
    for keywords <- combineAllLefts(keywords0)(SchemaProblems.combine)
    yield add(f(keywords))

  private def doneParsing(): Keywords =
    lastKeywords
      .foldLeft(this) { (acc, push) =>
        push(acc)
      }
      .copy(lastKeywords = Seq())

  private def withKeyword(
      keyword: String,
      value: Value,
      resolver: SchemaResolver,
      scope: DynamicScope
  ): Either[SchemaProblems, Keywords] =
    val scope1: DynamicScope        = scope.push(keyword)
    given location: CurrentLocation = scope1.currentLocation

    (keyword, value) match
      case ("type", StringValue(typeName)) =>
        Right(
          getTypeCheck(typeName)
            .map(add(_))
            .getOrElse(this)
        )

      case ("type", ArrayValue(values)) =>
        def typeNames = Value.asStrings(values)
        def keywords = typeNames
          .flatMap(getTypeCheck)
          .map(withLocation(_))

        Right(add(UnionTypeKeyword(keywords)))

      case ("not", value) =>
        for keywords <- Keywords.parseKeywords(vocabulary, resolver.push(SchemaValue(value)), scope1)
        yield add(NotKeyword(keywords))

      case ("items", value) =>
        for keywords <- Keywords.parseKeywords(vocabulary, resolver.push(SchemaValue(value)), scope1)
        yield updateKeyword(ArrayItemsKeyword())(check => check.copy(items = Some(keywords)))

      case ("prefixItems", ArrayValue(vs)) =>
        val keywords0 = vs.map(v => Keywords.parseKeywords(vocabulary, resolver.push(SchemaValue(v)), scope1))
        for keywords <- combineAllLefts(keywords0)(SchemaProblems.combine)
        yield updateKeyword(ArrayItemsKeyword())(check => check.copy(prefixItems = keywords))

      case ("unevaluatedItems", v) =>
        for keywords <- Keywords.parseKeywords(vocabulary, resolver.push(SchemaValue(v)), scope1)
        yield addLast(pushed => Keywords(vocabulary).add(UnevaluatedItemsKeyword(pushed, keywords)))

      case ("properties", ObjectValue(properties)) =>
        mapKeywordsFor(properties, resolver, scope1) { keywords =>
          updateKeyword(ObjectPropertiesKeyword())(keyword => keyword.copy(properties = keyword.properties ++ keywords))
        }

      case ("patternProperties", ObjectValue(properties)) =>
        mapKeywordsFor(properties, resolver, scope1) { keywords =>
          updateKeyword(ObjectPropertiesKeyword())(keyword => keyword.copy(patternProperties = keywords))
        }

      case ("additionalProperties", value) =>
        for keywords <- Keywords.parseKeywords(vocabulary, resolver.push(SchemaValue(value)), scope1)
        yield updateKeyword(ObjectPropertiesKeyword())(keyword => keyword.copy(additionalProperties = Some(keywords)))

      case ("unevaluatedProperties", v) =>
        for keywords <- Keywords.parseKeywords(vocabulary, resolver.push(SchemaValue(v)), scope1)
        yield addLast(pushed => Keywords(vocabulary).add(UnevaluatedPropertiesKeyword(pushed, keywords)))

      case ("required", ArrayValue(values)) =>
        def names = Value.asStrings(values)
        Right(add(ObjectRequiredKeyword(names)))

      case ("allOf", ArrayValue(values)) =>
        addAll(values.map(SchemaValue(_)), resolver, scope1)(AllOfKeyword.apply)

      case ("anyOf", ArrayValue(values)) =>
        addAll(values.map(SchemaValue(_)), resolver, scope1)(AnyOfKeyword.apply)

      case ("oneOf", ArrayValue(values)) =>
        addAll(values.map(SchemaValue(_)), resolver, scope1)(OneOfKeyword.apply)

      case ("if", value) =>
        updateKeywordsInside(resolver.push(SchemaValue(value)), scope1)(IfThenElseKeyword()) { (keywords, keyword) =>
          keyword.copy(ifKeywords = Some(keywords))
        }

      case ("then", value) =>
        updateKeywordsInside(resolver.push(SchemaValue(value)), scope1)(IfThenElseKeyword()) { (keywords, keyword) =>
          keyword.copy(thenKeywords = Some(keywords))
        }

      case ("else", value) =>
        updateKeywordsInside(resolver.push(SchemaValue(value)), scope1)(IfThenElseKeyword()) { (keywords, keyword) =>
          keyword.copy(elseKeywords = Some(keywords))
        }

      case ("enum", ArrayValue(values)) =>
        Right(add(EnumKeyword(values)))

      case ("const", value) =>
        Right(add(EnumKeyword(Seq(value))))

      case ("$id", StringValue(_)) =>
        // handled during load
        Right(this)

      case ("$anchor", StringValue(_)) =>
        // handled during load
        Right(this)

      case ("$dynamicAnchor", StringValue(_)) =>
        // handled during load
        Right(this)

      case ("$defs", ObjectValue(_)) =>
        // handled during load
        Right(this)

      case ("$ref", StringValue(ref)) =>
        Right(add(RefKeyword(ref, resolver.base, scope1)))

      case ("$dynamicRef", StringValue(ref)) =>
        Right(add(DynamicRefKeyword(ref, resolver.base, scope1)))

      case ("$comment", StringValue(_)) =>
        // only for schema authors and readers
        Right(this)

      case ("title", StringValue(_)) =>
        // ignore annotations
        Right(this)

      case ("default", _) =>
        // ignore annotations
        Right(this)

      case ("description", StringValue(_)) =>
        // ignore annotations
        Right(this)

      case ("pattern", StringValue(pattern)) =>
        Right(add(PatternKeyword(pattern)))

      case ("format", StringValue(format)) =>
        Right(add(FormatKeyword(format)))

      case ("minimum", NumberValue(v)) =>
        Right(add(MinimumKeyword(v)))

      case ("exclusiveMinimum", NumberValue(v)) =>
        Right(add(MinimumKeyword(v, exclude = true)))

      case ("minItems", NumberValue(v)) if v >= 0 =>
        Right(add(MinItemsKeyword(v)))

      case ("uniqueItems", BoolValue(v)) =>
        Right(add(UniqueItemsKeyword(v)))

      case ("propertyNames", value) =>
        for keywords <- Keywords.parseKeywords(vocabulary, resolver.push(SchemaValue(value)), scope1)
        yield add(PropertyNamesKeyword(keywords))

      case ("multipleOf", NumberValue(v)) if v > 0 =>
        Right(add(MultipleOfKeyword(v)))

      case ("maximum", NumberValue(v)) =>
        Right(add(MaximumKeyword(v)))

      case ("exclusiveMaximum", NumberValue(v)) =>
        Right(add(MaximumKeyword(v, exclude = true)))

      case ("maxLength", NumberValue(v)) if v >= 0 =>
        Right(add(MaxLengthKeyword(v)))

      case ("minLength", NumberValue(v)) if v >= 0 =>
        Right(add(MinLengthKeyword(v)))

      case ("maxItems", NumberValue(v)) if v >= 0 =>
        Right(add(MaxItemsKeyword(v)))

      case ("maxProperties", NumberValue(v)) if v >= 0 =>
        Right(add(MaxPropertiesKeyword(v)))

      case ("minProperties", NumberValue(v)) if v >= 0 =>
        Right(add(MinPropertiesKeyword(v)))

      case ("dependentRequired", ObjectValue(v)) =>
        val vv = v.view.flatMap {
          case (p, ArrayValue(vs)) =>
            Some(
              (
                p,
                vs.flatMap {
                  case StringValue(value) => Some(value)
                  case _                  => None
                }
              )
            )
          case _ => None
        }.toMap
        Right(add(DependentRequiredKeyword(vv)))

      case ("dependentSchemas", ObjectValue(ps)) =>
        val keywords0 = ps.view.map { case (p, v) =>
          Keywords
            .parseKeywords(vocabulary, resolver.push(SchemaValue(v)), scope1)
            .map(p -> _)
        }.toSeq
        for keywords <- combineAllLefts(keywords0)(SchemaProblems.combine).map(_.toMap)
        yield add(DependentSchemasKeyword(keywords))

      case ("contains", v) =>
        for keywords <- Keywords.parseKeywords(vocabulary, resolver.push(SchemaValue(v)), scope1)
        yield updateKeyword(ContainsKeyword())(check => check.copy(schema = Some(keywords)))

      case ("minContains", NumberValue(v)) if v >= 0 =>
        Right(updateKeyword(ContainsKeyword())(keyword => keyword.copy(min = Some(v.toInt))))

      case ("maxContains", NumberValue(v)) if v >= 0 =>
        Right(updateKeyword(ContainsKeyword())(keyword => keyword.copy(max = Some(v.toInt))))

      case ("$schema", v) =>
        // meta schema handled during load
        Right(this)

      case ("$vocabulary", _) =>
        // vocabulary handled earlier
        Right(this)

      // TODO
      // case ("deprecated", v) => {
      //   Right(this)
      // }

      case _ =>
        Left(SchemaProblems(UnsupportedKeyword(keyword)))

  private def mapKeywordsFor(props: Map[String, Value], resolver: SchemaResolver, scope: DynamicScope)(
      f: Map[String, Keywords] => Keywords
  ): Either[SchemaProblems, Keywords] =
    val propKeywords0 = props.view
      .map { case (prop, v) =>
        (prop, Keywords.parseKeywords(vocabulary, resolver.push(SchemaValue(v)), scope.push(prop)))
      }
      .map {
        case (prop, Right(keywords)) => Right((prop, keywords))
        case (prop, Left(problems))  => Left(problems.prefix(Pointer.empty / prop))
      }
      .toSeq
    for
      propKeywords <- combineAllLefts(propKeywords0)(SchemaProblems.combine)
      keywords = Map.from(propKeywords)
    yield f(keywords)

  private def updateKeyword[K <: Keyword](
      newKeyword: => K
  )(f: K => K)(using location: CurrentLocation, tt: TypeTest[Keyword, K]): Keywords =
    val keywords0: Set[Keyword] =
      if keywords.exists {
          case WithLocation(_, _: K) => true
          case _                     => false
        }
      then keywords
      else keywords + withLocation(newKeyword)
    this.copy(keywords = keywords0.map {
      case WithLocation(uri, keyword: K) => WithLocation(uri, f(keyword))
      case c @ _                         => c
    })

  private def updateKeywordsInside[K <: Keyword](
      resolution: SchemaResolution,
      scope: DynamicScope
  )(
      newKeyword: => K
  )(f: (Keywords, K) => K)(using CurrentLocation, TypeTest[Keyword, K]): Either[SchemaProblems, Keywords] =
    for keywords <- Keywords.parseKeywords(vocabulary, resolution, scope)
    yield updateKeyword(newKeyword)(f(keywords, _))

  private def getTypeCheck(typeName: String): Option[TypeKeyword] =
    typeName match
      case "null"    => Some(NullTypeKeyword)
      case "boolean" => Some(BooleanTypeKeyword)
      case "string"  => Some(StringTypeKeyword)
      case "number"  => Some(NumberTypeKeyword)
      case "integer" => Some(IntegerTypeKeyword)
      case "array"   => Some(ArrayTypeKeyword)
      case "object"  => Some(ObjectTypeKeyword)
      case _         => None

object Keywords:
  // type KeywordWithLocation = UriUtil.WithLocation[Keyword]

  def apply(
      schema: SchemaValue,
      vocabulary: Option[Vocabulary],
      lazyResolver: Option[LoadedSchemasResolver.LazyResolver]
  ): Either[SchemaProblems, Keywords] =
    val resolver: LoadedSchemasResolver = LoadedSchemasResolver(schema, lazyResolver)
    val scope                           = DynamicScope.empty.push(resolver.base)
    val parentVocabulary                = vocabulary.getOrElse(Vocabulary.coreVocabulary)
    for
      vocabulary <- SchemaValue.vocabulary(resolver.push(schema), parentVocabulary)
      keywords   <- Keywords.parseKeywords(vocabulary, resolver.push(schema), scope)
    yield keywords

  def parseKeywords(
      vocabulary: Vocabulary,
      resolution: SchemaResolution,
      scope: DynamicScope
  ): Either[SchemaProblems, Keywords] =
    val SchemaResolution(schema, resolver) = resolution
    val scope1: DynamicScope = SchemaValue
      .id(schema)
      .map(id => scope.push(resolver.absolute(id)))
      .getOrElse(scope)
    given CurrentLocation = scope1.currentLocation

    schema.value match
      case BoolValue(v) => Right(Keywords(vocabulary).add(TrivialKeyword(v)))
      case ObjectValue(properties) =>
        val keywords = Keywords(vocabulary)
        if properties.isEmpty then Right(keywords.add(TrivialKeyword(true)))
        else
          properties
            .foldLeft[Either[SchemaProblems, Keywords]](Right(keywords)) { case (keywords, (keyword, value)) =>
              val prefix = Pointer.empty / keyword
              accumulate(
                keywords,
                keywords
                  .flatMap { keywords =>
                    if vocabulary.defines(keyword) then {
                      keywords
                        .withKeyword(keyword, value, resolver, scope1)
                        .swap
                        .map(_.prefix(prefix))
                        .swap
                    } else {
                      Right(keywords.add(IgnoredKeyword(keyword)))
                    }
                  }
              )
            }
            .map(_.doneParsing())
      case _ => Left(SchemaProblems(InvalidSchemaValue(schema.value)))

  private def accumulate(
      previous: Either[SchemaProblems, Keywords],
      current: Either[SchemaProblems, Keywords]
  ): Either[SchemaProblems, Keywords] = (previous, current) match
    case (Right(_), Right(current))       => Right(current)
    case (Right(_), Left(problems))       => Left(problems)
    case (Left(previous), Left(problems)) => Left(previous.combine(problems))
    case (Left(previous), _)              => Left(previous)
