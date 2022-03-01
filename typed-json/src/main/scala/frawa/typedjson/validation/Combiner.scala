package frawa.typedjson.validation

import frawa.typedjson.keywords._
import frawa.typedjson.pointer.Pointer

trait Combiner[R] {
  def invalid(error: ValidationError, pointer: Pointer): Result[R]
  def valid(annotation: ValidationAnnotation, pointer: Pointer): Result[R]
  def allOf(results: Seq[Result[R]], pointer: Pointer): Result[R]
  def anyOf(results: Seq[Result[R]], pointer: Pointer): Result[R]
  def oneOf(results: Seq[Result[R]], pointer: Pointer): Result[R]
  def contains(results: Seq[Result[R]], pointer: Pointer, min: Option[Int], max: Option[Int]): Result[R]
  def not(results: Seq[Result[R]], pointer: Pointer): Result[R]
  def ifThenElse(results: Seq[Result[R]], pointer: Pointer): Result[R]
}
