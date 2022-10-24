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

package frawa.typedjson.validation

import frawa.typedjson.keywords.*
import frawa.typedjson.pointer.Pointer

trait Combiner[R]:
  def invalid(error: ValidationError, pointer: Pointer): Result[R]
  def valid(annotation: ValidationAnnotation, pointer: Pointer): Result[R]
  def allOf(results: Seq[Result[R]], pointer: Pointer): Result[R]
  def anyOf(results: Seq[Result[R]], pointer: Pointer): Result[R]
  def oneOf(results: Seq[Result[R]], pointer: Pointer): Result[R]
  def contains(results: Seq[Result[R]], pointer: Pointer, min: Option[Int], max: Option[Int]): Result[R]
  def not(results: Seq[Result[R]], pointer: Pointer): Result[R]
  def ifThenElse(results: Seq[Result[R]], pointer: Pointer): Result[R]
