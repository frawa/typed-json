package frawa.typedjson.eval

import frawa.typedjson.pointer.Pointer
import frawa.typedjson.validation.ValidationError
import frawa.typedjson.keywords.Evaluated
import frawa.typedjson.keywords.EvaluatedIndices
import frawa.typedjson.keywords.EvaluatedProperties
import frawa.typedjson.keywords.Keyword

trait OutputOps[O]: // extends Monoid[O]:
  def valid(pointer: Pointer): O
  def invalid(error: ValidationError, pointer: Pointer): O

  def all(os: Seq[O], pointer: Pointer): O

  extension (o: O)
    def not(pointer: Pointer): O
    def isValid: Boolean
    def withAnnotation(annotation: Evaluated): O
    def getAnnotations(): Seq[Evaluated]
    def forKeyword(k: Keyword): O

object OutputOps:
  def mergeAnnotations(es: Seq[Evaluated]): Seq[Evaluated] =
    val indices = es.flatMap {
      case EvaluatedIndices(indices) => indices
      case _                         => Seq()
    }.distinct
    val properties = es.flatMap {
      case EvaluatedProperties(properties) => properties
      case _                               => Set()
    }.toSet
    val es1 = if indices.nonEmpty then Seq(EvaluatedIndices(indices)) else Seq()
    val es2 = if properties.nonEmpty then Seq(EvaluatedProperties(properties)) else Seq()
    es1 ++ es2
