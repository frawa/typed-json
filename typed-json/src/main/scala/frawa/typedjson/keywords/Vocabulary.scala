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

import frawa.typedjson.parser.Value
import frawa.typedjson.parser.Value.*
import frawa.typedjson.util.SeqUtil
import frawa.typedjson.util.UriUtil.uri

import java.net.URI

case class Vocabulary(keywords: Map[String, Vocabulary.NestedSchemaType]):
  import Vocabulary.*

  def defines: String => Boolean = keywords.keySet.contains
  def nestedSchemas(keyword: String)(value: Value): Option[Seq[Value]] =
    keywords
      .get(keyword)
      .flatMap(nestedSchemasGetter)
      .map(_(value))

  def combine(other: Vocabulary): Vocabulary = this.copy(keywords = this.keywords ++ other.keywords)

object Vocabulary:

  trait NestedSchemaType
  case object NestedObjectSchemas extends NestedSchemaType
  case object NestedArraySchemas  extends NestedSchemaType
  case object NestedSelfSchema    extends NestedSchemaType
  case object NoNestedSchema      extends NestedSchemaType

  private type NestedSchemaGetter = Value => Seq[Value]

  private def objectSchemas: NestedSchemaGetter =
    case ObjectValue(ps) => ps.values.toSeq
    case _               => Seq.empty
  private def arraySchemas: NestedSchemaGetter =
    case ArrayValue(vs) => vs
    case _              => Seq.empty
  private def selfSchema: NestedSchemaGetter = v => Seq(v)

  def nestedSchemasGetter(t: NestedSchemaType): Option[NestedSchemaGetter] = t match
    case NestedObjectSchemas => Some(objectSchemas)
    case NestedArraySchemas  => Some(arraySchemas)
    case NestedSelfSchema    => Some(selfSchema)
    case _                   => None

  private val coreKeywords: Map[String, NestedSchemaType] = Map(
    "$id"            -> NoNestedSchema,
    "$schema"        -> NoNestedSchema,
    "$ref"           -> NoNestedSchema,
    "$anchor"        -> NoNestedSchema,
    "$dynamicRef"    -> NoNestedSchema,
    "$dynamicAnchor" -> NoNestedSchema,
    "$vocabulary"    -> NoNestedSchema,
    "$comment"       -> NoNestedSchema,
    "$defs"          -> NoNestedSchema
  )

  private val applicatorKeywords: Map[String, NestedSchemaType] = Map(
    "prefixItems"          -> NestedObjectSchemas,
    "items"                -> NestedSelfSchema,
    "contains"             -> NoNestedSchema,
    "additionalProperties" -> NestedSelfSchema,
    "properties"           -> NestedObjectSchemas,
    "patternProperties"    -> NestedObjectSchemas,
    "dependentSchemas"     -> NestedObjectSchemas,
    "propertyNames"        -> NestedSelfSchema,
    "if"                   -> NestedSelfSchema,
    "then"                 -> NestedSelfSchema,
    "else"                 -> NestedSelfSchema,
    "allOf"                -> NestedObjectSchemas,
    "anyOf"                -> NestedObjectSchemas,
    "oneOf"                -> NestedObjectSchemas,
    "not"                  -> NestedSelfSchema
  )

  private val contentKeywords: Map[String, NestedSchemaType] = Map(
    "contentEncoding"  -> NoNestedSchema,
    "contentMediaType" -> NoNestedSchema,
    "contentSchema"    -> NoNestedSchema
  )

  private val formatAnnotationKeywords: Map[String, NestedSchemaType] = Map(
    "format" -> NoNestedSchema
  )

  private val metaDataKeywords: Map[String, NestedSchemaType] = Map(
    "title"       -> NoNestedSchema,
    "description" -> NoNestedSchema,
    "default"     -> NoNestedSchema
    // TODO
//    "deprecated"  -> NoNestedSchema,
//    "readOnly"    -> NoNestedSchema,
//    "writeOnly"   -> NoNestedSchema,
//    "examples"    -> NoNestedSchema
  )

  private val unevaluatedKeywords: Map[String, NestedSchemaType] = Map(
    "unevaluatedItems"      -> NestedSelfSchema,
    "unevaluatedProperties" -> NestedSelfSchema
  )

  private val validationKeywords: Map[String, NestedSchemaType] = Map(
    "type"              -> NestedSelfSchema,
    "const"             -> NoNestedSchema,
    "enum"              -> NoNestedSchema,
    "multipleOf"        -> NoNestedSchema,
    "maximum"           -> NoNestedSchema,
    "exclusiveMaximum"  -> NoNestedSchema,
    "minimum"           -> NoNestedSchema,
    "exclusiveMinimum"  -> NoNestedSchema,
    "maxLength"         -> NoNestedSchema,
    "minLength"         -> NoNestedSchema,
    "pattern"           -> NoNestedSchema,
    "maxItems"          -> NoNestedSchema,
    "minItems"          -> NoNestedSchema,
    "uniqueItems"       -> NoNestedSchema,
    "maxContains"       -> NoNestedSchema,
    "minContains"       -> NoNestedSchema,
    "maxProperties"     -> NoNestedSchema,
    "minProperties"     -> NoNestedSchema,
    "required"          -> NoNestedSchema,
    "dependentRequired" -> NoNestedSchema
  )

  // in schema.json
  val deprecatedKeywords: Set[String] = Set(
    "definitions",
    "dependencies",
    "$recursiveAnchor",
    "$recursiveRef"
  )

  val coreId: URI             = uri("https://json-schema.org/draft/2020-12/vocab/core")
  val applicatorId: URI       = uri("https://json-schema.org/draft/2020-12/vocab/applicator")
  val contentId: URI          = uri("https://json-schema.org/draft/2020-12/vocab/content")
  val formatAnnotationId: URI = uri("https://json-schema.org/draft/2020-12/vocab/format-annotation")
  val metaDataId: URI         = uri("https://json-schema.org/draft/2020-12/vocab/meta-data")
  val unevaluatedId: URI      = uri("https://json-schema.org/draft/2020-12/vocab/unevaluated")
  val validationId: URI       = uri("https://json-schema.org/draft/2020-12/vocab/validation")

  val specVocabularies: Map[URI, Vocabulary] = Map(
    coreId             -> Vocabulary(coreKeywords),
    applicatorId       -> Vocabulary(applicatorKeywords),
    contentId          -> Vocabulary(contentKeywords),
    formatAnnotationId -> Vocabulary(formatAnnotationKeywords),
    metaDataId         -> Vocabulary(metaDataKeywords),
    unevaluatedId      -> Vocabulary(unevaluatedKeywords),
    validationId       -> Vocabulary(validationKeywords)
  )

  val coreVocabulary: Vocabulary = specVocabularies(coreId)

  def dialect(vocabularyIds: Map[URI, Boolean]): Either[SchemaProblems, Vocabulary] =
    val vocabularies = vocabularyIds
      .filter(_._2)
      .keys
      .map { id =>
        specVocabularies
          .get(id)
          .map(Right(_))
          .getOrElse(Left(SchemaProblems(SchemaProblems.UnknownRequiredVocabulary(id))))
      }
      .toSeq
    SeqUtil
      .sequenceAllLefts(vocabularies)
      .map(_.foldLeft(specVocabularies(coreId))(_.combine(_)))
      .swap
      .map(_.reduce(_.combine(_)))
      .swap

  def specDialect(): Vocabulary =
    specVocabularies.values.reduce(_.combine(_))
