package frawa.typedjson.output

import frawa.typedjson.pointer.Pointer
import frawa.typedjson.validation.ValidationError
import frawa.typedjson.keywords.Evaluated
import frawa.typedjson.keywords.EvaluatedIndices
import frawa.typedjson.keywords.EvaluatedProperties
import frawa.typedjson.keywords.Keyword
import frawa.typedjson.keywords.Ignored

trait OutputOps[O]: // extends Monoid[O]:
  def valid(pointer: Pointer): O
  def invalid(error: ValidationError, pointer: Pointer): O

  def all(os: Seq[O], pointer: Pointer): O

  extension (o: O)
    def not(pointer: Pointer): O
    def isValid: Boolean
    def withAnnotation(annotation: Evaluated): O = withAnnotations(Seq(annotation))
    def withAnnotations(annotations: Seq[Evaluated]): O
    def getAnnotations(): Seq[Evaluated]
    def forKeyword(k: Keyword): O

object OutputOps:
  def mergeEvaluatedAnnotations(es: Seq[Evaluated]): Seq[Evaluated] =
    val indices = es.flatMap {
      case EvaluatedIndices(indices) => indices
      case _                         => Set()
    }.toSet
    val properties = es.flatMap {
      case EvaluatedProperties(properties) => properties
      case _                               => Set()
    }.toSet
    val es1 = if indices.nonEmpty then Seq(EvaluatedIndices(indices)) else Seq()
    val es2 = if properties.nonEmpty then Seq(EvaluatedProperties(properties)) else Seq()
    es1 ++ es2

  def mergeAnnotations(es: Seq[Evaluated]): Seq[Evaluated] =
    val es1     = mergeEvaluatedAnnotations(es)
    val ignored = ignoredKeywords(es)
    val es2     = if ignored.nonEmpty then Seq(Ignored(ignored)) else Seq()
    es1 ++ es2

  def ignoredKeywords(es: Seq[Evaluated]): Set[String] =
    es.flatMap {
      case Ignored(keywords) => keywords
      case _                 => Set()
    }.toSet
