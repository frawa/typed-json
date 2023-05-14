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

import frawa.typedjson.pointer.Pointer
import frawa.typedjson.output.OutputOps

sealed trait Evaluated                                  extends OutputOps.Annotation
case class EvaluatedIndices(indices: Set[Int])          extends Evaluated
case class EvaluatedProperties(properties: Set[String]) extends Evaluated
case class Ignored(keywords: Set[String])               extends Evaluated
