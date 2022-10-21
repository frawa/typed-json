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

import frawa.typedjson.keywords.SchemaProblems.UnknownRequiredVocabulary
import frawa.typedjson.util.UriUtil.uri
import munit.FunSuite

class VocabularyTest extends FunSuite:
  import Vocabulary._

  test("spec vocabularies no duplicates") {
    val dups = specVocabularies.values
      .flatMap(_.keywords.view.mapValues(_ => 1).toSeq)
      .groupBy(_._1)
      .view
      .mapValues(_.map(_._2).sum)
      .filter(_._2 > 1)
      .keys
      .toSeq
    assertEquals(dups, Seq.empty)
  }

  test("empty dialect equals core") {
    assertEquals(Vocabulary.dialect(Map.empty), Right(specVocabularies(coreId)))
  }

  test("ignore optional unknown vocabulary") {
    assertEquals(Vocabulary.dialect(Map(uri("unknown") -> false)), Right(specVocabularies(coreId)))
  }

  test("fail on required unknown vocabulary") {
    val unknownId = uri("unknown")
    assertEquals(
      Vocabulary.dialect(Map(unknownId -> true)),
      Left(SchemaProblems(UnknownRequiredVocabulary(unknownId)))
    )
  }

  test("report all errors") {
    val unknownId  = uri("unknown")
    val unknownId2 = uri("unknown2")
    assertEquals(
      Vocabulary.dialect(Map(unknownId -> true, unknownId2 -> true, coreId -> true)),
      Left(
        SchemaProblems(
          Seq(
            WithPointer(UnknownRequiredVocabulary(unknownId)),
            WithPointer(UnknownRequiredVocabulary(unknownId2))
          )
        )
      )
    )
  }

  test("combine several") {
    val unknownId = uri("unknown")
    assertEquals(
      Vocabulary.dialect(Map(unknownId -> false, coreId -> true, validationId -> true)),
      Right(
        specVocabularies(coreId).combine(specVocabularies(validationId))
      )
    )
    assertEquals(
      Vocabulary.dialect(Map(unknownId -> false, coreId -> false, validationId -> true)),
      Right(
        specVocabularies(coreId).combine(specVocabularies(validationId))
      )
    )
    assertEquals(
      Vocabulary.dialect(Map(unknownId -> false, coreId -> false, validationId -> false)),
      Right(
        specVocabularies(coreId)
      )
    )
  }
