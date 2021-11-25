package frawa.typedjson.processor

import frawa.typedjson.parser.{ArrayValue, ObjectValue, Value}
import frawa.typedjson.util.SeqUtil
import frawa.typedjson.util.UriUtil.uri

import java.net.URI

case class Vocabulary(keywords: Map[String, Vocabulary.NestedSchemaType]) {
  import Vocabulary._

  def defines = keywords.keySet.contains _
  def nestedSchemas(keyword: String)(value: Value): Option[Seq[Value]] =
    keywords
      .get(keyword)
      .flatMap(nestedSchemasGetter)
      .map(_(value))

  def combine(other: Vocabulary): Vocabulary = this.copy(keywords = this.keywords ++ other.keywords)
}

object Vocabulary {

  trait NestedSchemaType
  case object NestedObjectSchemas extends NestedSchemaType
  case object NestedArraySchemas  extends NestedSchemaType
  case object NestedSelfSchema    extends NestedSchemaType
  case object NoNestedSchema      extends NestedSchemaType

  private type NestedSchemaGetter = Value => Seq[Value]

  private def objectSchemas: NestedSchemaGetter = { case ObjectValue(ps) => ps.values.toSeq }
  private def arraySchemas: NestedSchemaGetter  = { case ArrayValue(vs) => vs }
  private def selfSchema: NestedSchemaGetter    = v => Seq(v)

  def nestedSchemasGetter(t: NestedSchemaType): Option[NestedSchemaGetter] = t match {
    case NestedObjectSchemas => Some(objectSchemas)
    case NestedArraySchemas  => Some(arraySchemas)
    case NestedSelfSchema    => Some(selfSchema)
    case _                   => None
  }

  private val coreKeywords: Map[String, NestedSchemaType] = Map(
    "not"                   -> NestedSelfSchema,
    "items"                 -> NestedSelfSchema,
    "prefixItems"           -> NestedObjectSchemas,
    "unevaluatedItems"      -> NestedSelfSchema,
    "properties"            -> NestedObjectSchemas,
    "patternProperties"     -> NestedObjectSchemas,
    "additionalProperties"  -> NestedSelfSchema,
    "unevaluatedProperties" -> NestedSelfSchema,
    "allOf"                 -> NestedObjectSchemas,
    "anyOf"                 -> NestedObjectSchemas,
    "oneOf"                 -> NestedObjectSchemas,
    "if"                    -> NestedSelfSchema,
    "then"                  -> NestedSelfSchema,
    "else"                  -> NestedSelfSchema,
    "$defs"                 -> NestedObjectSchemas,
    "propertyNames"         -> NestedSelfSchema,
    "dependentSchemas"      -> NestedObjectSchemas,
    "contains"              -> NestedSelfSchema,
    "required"              -> NoNestedSchema,
    "$id"                   -> NoNestedSchema,
    "$anchor"               -> NoNestedSchema,
    "$dynamicAnchor"        -> NoNestedSchema,
    "$defs"                 -> NoNestedSchema,
    "$ref"                  -> NoNestedSchema,
    "$dynamicRef"           -> NoNestedSchema,
    "$comment"              -> NoNestedSchema,
    "title"                 -> NoNestedSchema,
    "default"               -> NoNestedSchema,
    "description"           -> NoNestedSchema,
    "propertyNames"         -> NoNestedSchema,
    "dependentSchemas"      -> NoNestedSchema,
    "contains"              -> NoNestedSchema,
    "$schema"               -> NoNestedSchema
    //    "$vocabulary"               -> NoNestedSchema,
  )

  private val validationKeywords: Map[String, NestedSchemaType] = Map(
    "type"              -> NestedSelfSchema,
    "enum"              -> NoNestedSchema,
    "const"             -> NoNestedSchema,
    "pattern"           -> NoNestedSchema,
    "format"            -> NoNestedSchema,
    "minimum"           -> NoNestedSchema,
    "exclusiveMinimum"  -> NoNestedSchema,
    "minItems"          -> NoNestedSchema,
    "uniqueItems"       -> NoNestedSchema,
    "multipleOf"        -> NoNestedSchema,
    "maximum"           -> NoNestedSchema,
    "exclusiveMaximum"  -> NoNestedSchema,
    "maxLength"         -> NoNestedSchema,
    "minLength"         -> NoNestedSchema,
    "maxItems"          -> NoNestedSchema,
    "maxProperties"     -> NoNestedSchema,
    "minProperties"     -> NoNestedSchema,
    "dependentRequired" -> NoNestedSchema,
    "minContains"       -> NoNestedSchema,
    "maxContains"       -> NoNestedSchema
  )

  val coreId       = uri("https://json-schema.org/draft/2020-12/vocab/core")
  val validationId = uri("https://json-schema.org/draft/2020-12/vocab/validation")

  val specVocabularies: Map[URI, Vocabulary] = Map(
    coreId       -> Vocabulary(coreKeywords),
    validationId -> Vocabulary(validationKeywords)
  )

  def dialect(vocabularyIds: Map[URI, Boolean]): Either[SchemaProblems, Vocabulary] = {
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
      .sequenceAllLefts2(vocabularies)
      .map(_.foldLeft(specVocabularies(coreId))(_.combine(_)))
      .swap
      .map(_.reduce(_.combine(_)))
      .swap
  }

}
