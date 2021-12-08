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

import frawa.typedjson.parser.ArrayValue
import frawa.typedjson.parser.BoolValue
import frawa.typedjson.parser.NumberValue
import frawa.typedjson.parser.ObjectValue
import frawa.typedjson.parser.StringValue
import frawa.typedjson.parser.Value
import frawa.typedjson.keywords.SchemaProblems.{
  InvalidSchemaValue,
  MissingDynamicReference,
  MissingReference,
  UnsupportedKeyword
}
import frawa.typedjson.util.UriUtil

import java.net.URI
import scala.reflect.ClassTag

sealed trait Keyword
sealed trait AssertionKeyword  extends Keyword
sealed trait ApplicatorKeyword extends Keyword

case class TrivialKeyword(v: Boolean)                                    extends AssertionKeyword
sealed trait TypeKeyword                                                 extends AssertionKeyword
case object NullTypeKeyword                                              extends TypeKeyword
case object BooleanTypeKeyword                                           extends TypeKeyword
case object StringTypeKeyword                                            extends TypeKeyword
case object NumberTypeKeyword                                            extends TypeKeyword
case object IntegerTypeKeyword                                           extends TypeKeyword
case object ArrayTypeKeyword                                             extends TypeKeyword
case object ObjectTypeKeyword                                            extends TypeKeyword
case class ObjectRequiredKeyword(names: Seq[String])                     extends AssertionKeyword
case class NotKeyword(keywords: Keywords)                                extends ApplicatorKeyword
case class AllOfKeyword(keywords: Seq[Keywords])                         extends ApplicatorKeyword
case class AnyOfKeyword(keywords: Seq[Keywords])                         extends ApplicatorKeyword
case class OneOfKeyword(keywords: Seq[Keywords])                         extends ApplicatorKeyword
case class UnionTypeKeyword(keywords: Seq[Keywords.KeywordWithLocation]) extends ApplicatorKeyword
case class EnumKeyword(values: Seq[Value])                               extends AssertionKeyword
case class ArrayItemsKeyword(
    items: Option[Keywords] = None,
    prefixItems: Seq[Keywords] = Seq()
) extends ApplicatorKeyword
case class ObjectPropertiesKeyword(
    properties: Map[String, Keywords] = Map(),
    patternProperties: Map[String, Keywords] = Map(),
    additionalProperties: Option[Keywords] = None
) extends ApplicatorKeyword
case class IfThenElseKeyword(
    ifKeywords: Option[Keywords] = None,
    thenKeywords: Option[Keywords] = None,
    elseKeywords: Option[Keywords] = None
) extends ApplicatorKeyword
case class PatternKeyword(pattern: String)                                                 extends AssertionKeyword
case class FormatKeyword(format: String)                                                   extends AssertionKeyword
case class MinimumKeyword(min: BigDecimal, exclude: Boolean = false)                       extends AssertionKeyword
case class UniqueItemsKeyword(unique: Boolean)                                             extends AssertionKeyword
case class PropertyNamesKeyword(keywords: Keywords)                                        extends ApplicatorKeyword
case class LazyParseKeywords(resolved: URI, parse: () => Either[SchemaProblems, Keywords]) extends ApplicatorKeyword
case class MultipleOfKeyword(n: BigDecimal)                                                extends AssertionKeyword
case class MaximumKeyword(max: BigDecimal, exclude: Boolean = false)                       extends AssertionKeyword
case class MaxLengthKeyword(max: BigDecimal)                                               extends AssertionKeyword
case class MinLengthKeyword(min: BigDecimal)                                               extends AssertionKeyword
case class MaxItemsKeyword(max: BigDecimal)                                                extends AssertionKeyword
case class MinItemsKeyword(min: BigDecimal)                                                extends AssertionKeyword
case class MaxPropertiesKeyword(max: BigDecimal)                                           extends AssertionKeyword
case class MinPropertiesKeyword(min: BigDecimal)                                           extends AssertionKeyword
case class DependentRequiredKeyword(required: Map[String, Seq[String]])                    extends AssertionKeyword
case class DependentSchemasKeyword(keywords: Map[String, Keywords])                        extends ApplicatorKeyword
case class ContainsKeyword(schema: Option[Keywords] = None, min: Option[Int] = None, max: Option[Int] = None)
    extends ApplicatorKeyword
case class UnevaluatedItemsKeyword(pushed: Keywords, unevaluated: Keywords)      extends ApplicatorKeyword
case class UnevaluatedPropertiesKeyword(pushed: Keywords, unevaluated: Keywords) extends ApplicatorKeyword

case class Keywords(
    vocabulary: Vocabulary,
    schema: SchemaValue,
    keywords: Seq[Keywords.KeywordWithLocation] = Seq.empty[Keywords.KeywordWithLocation],
    ignored: Set[String] = Set.empty
) {
  import frawa.typedjson.util.SeqUtil._
  import Keywords._

  private def add(keyword: KeywordWithLocation): Keywords =
    this.copy(keywords = keywords :+ keyword)

  // TODO avoid implicit?
  private def addAll(
      schemas: Seq[SchemaValue],
      scope: DynamicScope
  )(
      f: Seq[Keywords] => Keyword
  )(implicit resolver: SchemaResolver): Either[SchemaProblems, Keywords] = {
    val keywords0 = schemas.zipWithIndex.map { case (v, i) =>
      Keywords.parseKeywords(vocabulary, resolver.push(v), scope.push(i))
    }
    for {
      keywords <- combineAllLefts(keywords0)(SchemaProblems.combine)
    } yield {
      add(withLocation(f(keywords))(scope)).withIgnored(keywords.flatMap(_.ignored).toSet)
    }
  }

  // TODO avoid implicit?
  private def withKeyword(keyword: String, value: Value, scope: DynamicScope)(implicit
      resolver: SchemaResolver
  ): Either[SchemaProblems, Keywords] = {
    implicit val scope1: DynamicScope = scope.push(keyword)

    (keyword, value) match {
      case ("type", StringValue(typeName)) =>
        Right(
          getTypeCheck(typeName)
            .map(add(_))
            .getOrElse(withIgnored(s"$keyword-$typeName"))
        )

      case ("type", ArrayValue(values)) =>
        def typeNames = Value.asStrings(values)
        def keywords = typeNames
          .flatMap(getTypeCheck)
          .map(withLocation(_))

        Right(add(UnionTypeKeyword(keywords)))

      case ("not", value) =>
        for {
          keywords <- Keywords.parseKeywords(vocabulary, resolver.push(SchemaValue(value)), scope1)
        } yield {
          add(NotKeyword(keywords))
        }

      case ("items", value) =>
        for {
          keywords <- Keywords.parseKeywords(vocabulary, resolver.push(SchemaValue(value)), scope1)
        } yield {
          updateKeyword(ArrayItemsKeyword())(check => check.copy(items = Some(keywords)))
        }

      case ("prefixItems", ArrayValue(vs)) =>
        val keywords0 = vs.map(v => Keywords.parseKeywords(vocabulary, resolver.push(SchemaValue(v)), scope1))
        for {
          keywords <- combineAllLefts(keywords0)(SchemaProblems.combine)
        } yield {
          updateKeyword(ArrayItemsKeyword())(check => check.copy(prefixItems = keywords))
        }

      case ("unevaluatedItems", v) =>
        for {
          keywords <- Keywords.parseKeywords(vocabulary, resolver.push(SchemaValue(v)), scope1)
        } yield {
          Keywords(vocabulary, schema).add(UnevaluatedItemsKeyword(this, keywords))
        }

      case ("properties", ObjectValue(properties)) =>
        mapKeywordsFor(properties, scope1) { keywords =>
          updateKeyword(ObjectPropertiesKeyword())(keyword => keyword.copy(properties = keyword.properties ++ keywords))
        }

      case ("patternProperties", ObjectValue(properties)) =>
        mapKeywordsFor(properties, scope1) { keywords =>
          updateKeyword(ObjectPropertiesKeyword())(keyword => keyword.copy(patternProperties = keywords))
        }

      case ("additionalProperties", value) =>
        for {
          keywords <- Keywords.parseKeywords(vocabulary, resolver.push(SchemaValue(value)), scope1)
        } yield {
          updateKeyword(ObjectPropertiesKeyword())(keyword => keyword.copy(additionalProperties = Some(keywords)))
        }

      case ("unevaluatedProperties", v) =>
        for {
          keywords <- Keywords.parseKeywords(vocabulary, resolver.push(SchemaValue(v)), scope1)
        } yield {
          Keywords(vocabulary, schema).add(UnevaluatedPropertiesKeyword(this, keywords))
        }

      case ("required", ArrayValue(values)) =>
        def names = Value.asStrings(values)

        Right(add(ObjectRequiredKeyword(names)))

      case ("allOf", ArrayValue(values)) =>
        addAll(values.map(SchemaValue(_)), scope1)(AllOfKeyword)

      case ("anyOf", ArrayValue(values)) =>
        addAll(values.map(SchemaValue(_)), scope1)(AnyOfKeyword)

      case ("oneOf", ArrayValue(values)) =>
        addAll(values.map(SchemaValue(_)), scope1)(OneOfKeyword)

      case ("if", value) =>
        updateKeywordsInside(resolver.push(SchemaValue(value)))(IfThenElseKeyword()) { (keywords, keyword) =>
          keyword.copy(ifKeywords = Some(keywords))
        }

      case ("then", value) =>
        updateKeywordsInside(resolver.push(SchemaValue(value)))(IfThenElseKeyword()) { (keywords, keyword) =>
          keyword.copy(thenKeywords = Some(keywords))
        }

      case ("else", value) =>
        updateKeywordsInside(resolver.push(SchemaValue(value)))(IfThenElseKeyword()) { (keywords, keyword) =>
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
        for {
          resolution <- resolver
            .resolveRef(ref)
            .map(Right(_))
            .getOrElse(Left(SchemaProblems(MissingReference(ref))))
          vocabulary1 <- SchemaValue.vocabulary(resolution, vocabulary)
          keyword = lazyResolve(vocabulary1, resolution, scope1)
        } yield {
          add(keyword)
        }

      case ("$dynamicRef", StringValue(ref)) =>
        for {
          resolution <- resolver
            .resolveDynamicRef(ref, scope)
            .map(Right(_))
            .getOrElse(Left(SchemaProblems(MissingDynamicReference(ref))))
          vocabulary1 <- SchemaValue.vocabulary(resolution, vocabulary)
          keyword = lazyResolve(vocabulary1, resolution, scope1)
        } yield {
          add(keyword)
        }

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
        for {
          keywords <- Keywords.parseKeywords(vocabulary, resolver.push(SchemaValue(value)), scope1)
        } yield {
          add(PropertyNamesKeyword(keywords))
        }

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
        for {
          keywords <- combineAllLefts(keywords0)(SchemaProblems.combine).map(_.toMap)
        } yield {
          add(DependentSchemasKeyword(keywords))
        }

      case ("contains", v) =>
        for {
          keywords <- Keywords.parseKeywords(vocabulary, resolver.push(SchemaValue(v)), scope1)
        } yield {
          updateKeyword(ContainsKeyword())(check => check.copy(schema = Some(keywords)))
        }

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

      case _ => Left(SchemaProblems(UnsupportedKeyword(keyword)))
    }
  }

  private def lazyResolve(
      vocabulary: Vocabulary,
      resolution: SchemaResolver.Resolution,
      scope: DynamicScope
  ): LazyParseKeywords = {
    val resolveLater = { () =>
      Keywords.parseKeywords(vocabulary, resolution, scope)
    }
    val resolved = resolution._2.base
    LazyParseKeywords(resolved, resolveLater)
  }

  // TODO avoid implicit?
  private def mapKeywordsFor(
      props: Map[String, Value],
      scope: DynamicScope
  )(f: Map[String, Keywords] => Keywords)(implicit resolver: SchemaResolver): Either[SchemaProblems, Keywords] = {
    val propKeywords0 = props.view
      .map { case (prop, v) =>
        (prop, Keywords.parseKeywords(vocabulary, resolver.push(SchemaValue(v)), scope.push(prop)))
      }
      .map {
        case (prop, Right(keywords)) => Right((prop, keywords))
        case (prop, Left(problems))  => Left(problems.prefix(Pointer.empty / prop))
      }
      .toSeq
    for {
      propKeywords <- combineAllLefts(propKeywords0)(SchemaProblems.combine)
      keywords = Map.from(propKeywords)
      ignored  = keywords.values.flatMap(_.ignored).toSet
    } yield {
      f(keywords).withIgnored(ignored)
    }
  }

  // TODO avoid implicit?
  private def updateKeyword[K <: Keyword: ClassTag](
      newKeyword: => K
  )(f: K => K)(implicit scope: DynamicScope): Keywords = {
    val keywords0: Seq[KeywordWithLocation] =
      if (
        keywords.exists {
          case UriUtil.WithLocation(_, _: K) => true
          case _                             => false
        }
      ) {
        keywords
      } else {
        keywords :+ withLocation(newKeyword)
      }
    this.copy(keywords = keywords0.map {
      case UriUtil.WithLocation(uri, keyword: K) => UriUtil.WithLocation(uri, f(keyword))
      case c @ _                                 => c
    })
  }

  private def updateKeywordsInside[K <: Keyword: ClassTag](
      resolution: SchemaResolver.Resolution
  )(
      newKeyword: => K
  )(f: (Keywords, K) => K)(implicit scope: DynamicScope): Either[SchemaProblems, Keywords] = {
    for {
      keywords <- Keywords.parseKeywords(vocabulary, resolution, scope)
    } yield {
      updateKeyword(newKeyword)(f(keywords, _))
    }
  }

  private def withIgnored(keyword: String): Keywords =
    this.copy(ignored = ignored + keyword)

  private def withIgnored(ignored: Set[String]): Keywords =
    this.copy(ignored = ignored.concat(ignored))

  private def getTypeCheck(typeName: String): Option[TypeKeyword] =
    typeName match {
      case "null"    => Some(NullTypeKeyword)
      case "boolean" => Some(BooleanTypeKeyword)
      case "string"  => Some(StringTypeKeyword)
      case "number"  => Some(NumberTypeKeyword)
      case "integer" => Some(IntegerTypeKeyword)
      case "array"   => Some(ArrayTypeKeyword)
      case "object"  => Some(ObjectTypeKeyword)
      case _         => None
    }
}

object Keywords {
  type KeywordWithLocation = UriUtil.WithLocation[Keyword]

  import scala.language.implicitConversions

  private implicit def withLocation(keyword: Keyword)(implicit scope: DynamicScope): KeywordWithLocation = {
    import frawa.typedjson.util.UriUtil._
    scope.uris.lastOption.map(WithLocation(_, keyword)).getOrElse(WithLocation(uri("#"), keyword))
  }

  def apply(
      schema: SchemaValue,
      vocabulary: Option[Vocabulary],
      lazyResolver: Option[LoadedSchemasResolver.LazyResolver]
  ): Either[SchemaProblems, Keywords] = {
    val resolver: LoadedSchemasResolver = LoadedSchemasResolver(schema, lazyResolver)
    val scope                           = DynamicScope.empty.push(resolver.base)
    val parentVocabulary                = vocabulary.getOrElse(Vocabulary.coreVocabulary)
    for {
      vocabulary <- SchemaValue.vocabulary(resolver.push(schema), parentVocabulary)
      keywords   <- Keywords.parseKeywords(vocabulary, resolver.push(schema), scope)
    } yield keywords
  }

  def parseKeywords(
      vocabulary: Vocabulary,
      resolution: SchemaResolver.Resolution,
      scope: DynamicScope
  ): Either[SchemaProblems, Keywords] = {
    val schema    = resolution._1
    val resolver1 = resolution._2
    implicit val scope1: DynamicScope = SchemaValue
      .id(schema)
      .map(id => scope.push(resolver1.absolute(id)))
      .getOrElse(scope)

    schema.value match {
      case BoolValue(v) => Right(Keywords(vocabulary, schema).add(TrivialKeyword(v)))
      case ObjectValue(properties) =>
        val keywords = Keywords(vocabulary, schema)
        if (properties.isEmpty) {
          Right(keywords.add(TrivialKeyword(true)))
        } else {
          properties
            .foldLeft[Either[SchemaProblems, Keywords]](Right(keywords)) { case (keywords, (keyword, value)) =>
              val prefix = Pointer.empty / keyword
              accumulate(
                keywords,
                keywords
                  .flatMap { keywords =>
                    if (vocabulary.defines(keyword)) {
                      keywords
                        .withKeyword(keyword, value, scope1)(resolver1)
                        .swap
                        .map(_.prefix(prefix))
                        .swap
                    } else {
                      Right(keywords.withIgnored(keyword))
                    }
                  }
              )
            }
        }
      case _ => Left(SchemaProblems(InvalidSchemaValue(schema.value)))
    }
  }

  private def accumulate(
      previous: Either[SchemaProblems, Keywords],
      current: Either[SchemaProblems, Keywords]
  ): Either[SchemaProblems, Keywords] = (previous, current) match {
    case (Right(_), Right(current))       => Right(current)
    case (Right(_), Left(problems))       => Left(problems)
    case (Left(previous), Left(problems)) => Left(previous.combine(problems))
    case (Left(previous), _)              => Left(previous)
  }
}
