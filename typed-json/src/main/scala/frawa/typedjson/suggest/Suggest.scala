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

package frawa.typedjson.suggest

import frawa.typedjson.keywords.Keyword
import frawa.typedjson.keywords._
import frawa.typedjson.parser.Value
import frawa.typedjson.parser.Value._
import frawa.typedjson.pointer.Pointer

import java.net.URI
import scala.collection.immutable.Seq
import frawa.typedjson.parser.Offset

case class SuggestResult(suggestions: Seq[Suggest])

enum Suggest:
  def values: Seq[Value] = this match {
    case Values(vs)        => vs
    case WithDoc(s, _)     => s.values
    case WithReplace(s, _) => s.values
  }
  case Values(vs: Seq[Value])
  case WithDoc(s: Suggest, doc: Suggest.Doc)
  case WithReplace(s: Suggest, replace: Offset)

object Suggest:
  // TODO deprecated, readOnly, writeOnly
  case class Doc(
      id: Option[URI] = None,
      title: Option[String] = None,
      description: Option[String] = None
  )

  def suggestAt[R[_], O](at: Pointer)(compiled: Value => R[O]): Value => R[O] =
    // TODO stop evaluation as soon as 'at' is reached
    compiled

  def suggestions(at: Pointer, keysOnly: Boolean, output: SuggestOutput): SuggestResult =
    val ws = output.keywords.map(suggestFor)
    val metaByLoc = output.keywords
      .groupBy {
        case WithLocation(meta: MetaKeyword, kl) => Some(kl.parent)
        case _: MetaKeyword                      => Some(KeywordLocation.empty)
        case _                                   => None
      }
      .filter(_._1.isDefined)
      .flatMap {
        case (Some(kl), metas) =>
          metas
            .flatMap {
              case WithLocation(meta: MetaKeyword, _) => Some(meta)
              case meta: MetaKeyword                  => Some(meta)
              case _                                  => None
            }
            .headOption
            .map((kl, _))
        case _ => None
      }
      .toMap
    val suggestions = ws
      .groupBy(Work.parentLoc)
      .view
      .mapValues(_.flatMap(_.values).distinct)
      .map(toSuggest(metaByLoc.get))
      .toSeq
    // TODO use more precise replaceAt for better suggestions
    if keysOnly then SuggestResult(suggestions.map(onlyKeys))
    else SuggestResult(suggestions)

  private def toSuggest(
      findMeta: KeywordLocation => Option[MetaKeyword]
  )(parent: Option[KeywordLocation], vs: Seq[Value]): Suggest =
    parent
      .flatMap(findMeta)
      .flatMap(toDoc(parent, _))
      .map { doc =>
        Suggest.WithDoc(Suggest.Values(vs), doc)
      }
      .getOrElse(Suggest.Values(vs))

  private def toDoc(kl: Option[KeywordLocation], meta: MetaKeyword): Option[Doc] =
    val location = kl flatMap {
      case KeywordLocation.Dereferenced(_, absolute) => Some(absolute)
      case _                                         => None
    }
    if Seq(meta.title, meta.description).exists(_.isDefined) then
      Some(Doc(location, meta.title, meta.description))
    else None

  private enum Work(vs: Seq[Value]):
    def values: Seq[Value] = vs

    case Vals(vs: Seq[Value])                         extends Work(vs)
    case WithLoc(vs: Seq[Value], kl: KeywordLocation) extends Work(vs)

  private object Work {
    def apply(v: Value): Work =
      Work.Vals(Seq(v))
    def apply(vs: Seq[Value]): Work =
      Work.Vals(vs.distinct)
    def all(ws: Seq[Work]): Work =
      Work.Vals(ws.flatMap(_.values).distinct)

    def parentLoc(w: Work): Option[KeywordLocation] = w match {
      case _: Vals        => None
      case WithLoc(_, kl) => Some(kl.parent)
    }
  }

  private def suggestFor(keyword: Keyword): Work =
    keyword match
      case NullTypeKeyword    => Work(NullValue)
      case BooleanTypeKeyword => Work(BoolValue(true))
      case StringTypeKeyword  => Work(StringValue(""))
      case NumberTypeKeyword  => Work(NumberValue(0.0))
      case IntegerTypeKeyword => Work(NumberValue(0))
      case ArrayTypeKeyword   => Work(ArrayValue(Seq()))
      case ObjectTypeKeyword  => Work(ObjectValue(Map()))
      case ObjectPropertiesKeyword(properties, patternProperties, additional) =>
        val allProperties = ObjectValue(
          Map.from(
            properties
              .map { case (prop, keywords) =>
                useBestValue(
                  keywords.flatMap(keyword => suggestFor(keyword).values)
                )
                  .map(v => (prop -> v))
                  .getOrElse {
                    // TODO happens for $ref/$dynamicRef
                    // TODO find deref
                    prop -> NullValue
                  }
              }
              .toSeq
              .sortBy(_._1)
          )
        )
        val allPatternProperties = ObjectValue(
          Map.from(
            patternProperties
              .flatMap { case (prop, keywords) =>
                useBestValue(
                  keywords.flatMap(keyword => suggestFor(keyword).values)
                ).map(v => (prop -> v))
              }
              .toSeq
              .sortBy(_._1)
          )
        )
        val additionals =
          additional.flatMap { additional =>
            useBestValue(additional.keywords.toSeq.flatMap(keyword => suggestFor(keyword).values))
          }.toSeq
        Work(Seq(allProperties, allPatternProperties) ++ additionals)
      case ObjectRequiredKeyword(required) =>
        Work(ObjectValue(Map.from(required.map((_, NullValue)))))
      case TrivialKeyword(v) => Work(BoolValue(v))
      case IfThenElseKeyword(ifChecks, thenChecks, elseChecks) =>
        Work.all(
          Seq(ifChecks, thenChecks, elseChecks).flatten
            .flatMap(_.map(keyword => suggestFor(keyword)))
        )
      case OneOfKeyword(keywords) =>
        Work.all(
          keywords.flatMap(_.map(keyword => suggestFor(keyword)))
        )
      case AnyOfKeyword(keywords) =>
        Work.all(
          keywords.flatMap(_.map(keyword => suggestFor(keyword)))
        )
      case AllOfKeyword(keywords) =>
        // TODO intersect?
        Work.all(
          keywords.flatMap(_.map(keyword => suggestFor(keyword)))
        )
      case EnumKeyword(values) => Work(values)
      case ArrayItemsKeyword(items, prefixItems) =>
        val itemArrays = Seq(items).flatten
          .flatMap(ks => useBestValue(ks.flatMap(keyword => suggestFor(keyword).values)))
          .map(v => ArrayValue(Seq(v)))
        val tuplesOfDeeptest = ArrayValue(
          prefixItems
            .map(ks => useBestValue(ks.flatMap(keyword => suggestFor(keyword).values)))
            .map(_.getOrElse(NullValue))
        )
        Work(itemArrays :+ tuplesOfDeeptest)
      case UnionTypeKeyword(keywords) =>
        Work.all(keywords.map(keyword => suggestFor(keyword)))
      case ContentSchemaKeyword(keywords) =>
        Work.all(keywords.map(keyword => suggestFor(keyword)))
      case MetaKeyword(_, _, Some(defaultValue), _, _, _, _) =>
        Work(defaultValue)
      case MetaKeyword(_, _, _, _, _, _, Some(examples)) =>
        Work(examples)
      case WithLocation(keyword, kl) =>
        Work.WithLoc(suggestFor(keyword).values, kl)
      case MinimumKeyword(min, exclude) =>
        val v = if exclude then min + 1 else min
        Work(NumberValue(v))
      case MaximumKeyword(max, exclude) =>
        val v = if exclude then max - 1 else max
        Work(NumberValue(v))
      case MultipleOfKeyword(n)            => Work(NumberValue(n))
      case _: FormatKeyword                => Work(StringValue(""))
      case _: PatternKeyword               => Work(StringValue(""))
      case _: UniqueItemsKeyword           => Work(ArrayValue(Seq()))
      case _: PropertyNamesKeyword         => Work(ObjectValue(Map()))
      case _: MaxLengthKeyword             => Work(StringValue(""))
      case _: MinLengthKeyword             => Work(StringValue(""))
      case _: MaxItemsKeyword              => Work(ArrayValue(Seq()))
      case _: MinItemsKeyword              => Work(ArrayValue(Seq()))
      case _: MaxPropertiesKeyword         => Work(ObjectValue(Map()))
      case _: MinPropertiesKeyword         => Work(ObjectValue(Map()))
      case _: DependentRequiredKeyword     => Work(ObjectValue(Map()))
      case _: DependentSchemasKeyword      => Work(ObjectValue(Map()))
      case _: ContainsKeyword              => Work(ArrayValue(Seq()))
      case _: ContentEncodingKeyword       => Work(Seq())
      case _: ContentMediaTypeKeyword      => Work(Seq())
      case _: UnevaluatedItemsKeyword      => Work(Seq())
      case _: UnevaluatedPropertiesKeyword => Work(Seq())
      case _: NotKeyword                   => Work(Seq())
      case _: RefKeyword                   => Work(Seq())
      case _: DynamicRefKeyword            => Work(Seq())
      case _: MetaKeyword                  => Work(Seq())
      case _: IgnoredKeyword               => Work(Seq())

  private def useBestValue(vs: Seq[Value]): Option[Value] =
    vs.sortBy(score).lastOption

  private def onlyKeys(w: Suggest): Suggest =
    w match {
      case Suggest.Values(vs)            => Suggest.Values(onlyKeys(vs))
      case Suggest.WithDoc(s, doc)       => Suggest.WithDoc(onlyKeys(s), doc)
      case Suggest.WithReplace(s, range) => Suggest.WithReplace(onlyKeys(s), range)
    }

  private def onlyKeys(vs: Seq[Value]): Seq[Value] =
    vs.flatMap(Value.asObject)
      .flatMap { properties =>
        val keys = properties.keys.toSeq.map(StringValue.apply)
        val deep = onlyKeys(properties.values.toSeq)
        keys ++ deep
      }
      .distinct

  def score(v: Value): Int =
    v match
      case ArrayValue(vs)  => 2 + vs.map(score).maxOption.map(_ + 1).getOrElse(0)
      case ObjectValue(ps) => 2 + ps.values.map(score).maxOption.map(_ + 1).getOrElse(0)
      case NumberValue(v)  => if v != 0 then 2 else 1
      case StringValue(v)  => if v.nonEmpty then 2 else 1
      case BoolValue(_)    => 1
      case NullValue       => 0
