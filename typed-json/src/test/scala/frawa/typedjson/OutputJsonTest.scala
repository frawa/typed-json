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

package frawa.typedjson

import munit.FunSuite

import frawa.typedjson.parser.Value._
import frawa.typedjson.validation.ValidationOutput
import frawa.typedjson.keywords.Result
import frawa.typedjson.validation.FalseSchemaReason
import frawa.typedjson.parser.jawn.JawnParser

class OutputJsonTest extends FunSuite {

  test("flag valid") {
    assertEquals(
      OutputJson.flag(TypedJson.Validation(true, TypedJson.Output(Nil))),
      ObjectValue(Map("valid" -> BoolValue(true)))
    )
  }

  test("flag invalid") {
    assertEquals(
      OutputJson.flag(TypedJson.Validation(false, TypedJson.Output(Nil))),
      ObjectValue(Map("valid" -> BoolValue(false)))
    )
  }

  test("basic valid") {
    assertEquals(
      OutputJson.basic(TypedJson.Validation(true, TypedJson.Output(Nil))),
      ObjectValue(Map("valid" -> BoolValue(true)))
    )
  }

  test("basic errors") {
    assertEquals(
      OutputJson.basic(
        TypedJson.Validation(
          false,
          TypedJson.Output(Result.valid(ValidationOutput.invalid(FalseSchemaReason()))(ValidationOutput.add))
        )
      ),
      ObjectValue(
        Map(
          "valid" -> BoolValue(false),
          "errors" -> ArrayValue(
            Seq(
              ObjectValue(
                properties = Map(
                  "error"            -> StringValue("FalseSchemaReason()"),
                  "instanceLocation" -> StringValue("")
                )
              )
            )
          )
        )
      )
    )
  }

  test("basic sample".ignore) {
    implicit val p = new JawnParser()

    val typedJson  = TypedJson.create(Sample.schema).toOption.get
    val validation = typedJson.validate(Sample.value)
    val basic      = validation.map(OutputJson.basic).toOption

    val expected = p.parse(Sample.expectedBasic).toOption
    assertEquals(validation, null)
    assertEquals(basic, expected)
  }

}

object Sample {
  val schema: String = """|{
                  |  "$id": "https://example.com/polygon",
                  |  "$schema": "https://json-schema.org/draft/2020-12/schema",
                  |  "$defs": {
                  |    "point": {
                  |      "type": "object",
                  |      "properties": {
                  |        "x": { "type": "number" },
                  |        "y": { "type": "number" }
                  |      },
                  |      "additionalProperties": false,
                  |      "required": [ "x", "y" ]
                  |    }
                  |  },
                  |  "type": "array",
                  |  "items": { "$ref": "#/$defs/point" },
                  |  "minItems": 3
                  |}
                  |""".stripMargin

  val value: String = """|[
                 |  {
                 |    "x": 2.5,
                 |    "y": 1.3
                 |  },
                 |  {
                 |    "x": 1,
                 |    "z": 6.7
                 |  }
                 |]
                 |""".stripMargin

  val expectedBasic: String = """|{
                         |  "valid": false,
                         |  "errors": [
                         |    {
                         |      "keywordLocation": "",
                         |      "instanceLocation": "",
                         |      "error": "A subschema had errors."
                         |    },
                         |    {
                         |      "keywordLocation": "/items/$ref",
                         |      "absoluteKeywordLocation":
                         |        "https://example.com/polygon#/$defs/point",
                         |      "instanceLocation": "/1",
                         |      "error": "A subschema had errors."
                         |    },
                         |    {
                         |      "keywordLocation": "/items/$ref/required",
                         |      "absoluteKeywordLocation":
                         |        "https://example.com/polygon#/$defs/point/required",
                         |      "instanceLocation": "/1",
                         |      "error": "Required property 'y' not found."
                         |    },
                         |    {
                         |      "keywordLocation": "/items/$ref/additionalProperties",
                         |      "absoluteKeywordLocation":
                         |        "https://example.com/polygon#/$defs/point/additionalProperties",
                         |      "instanceLocation": "/1/z",
                         |      "error": "Additional property 'z' found but was invalid."
                         |    },
                         |    {
                         |      "keywordLocation": "/minItems",
                         |      "instanceLocation": "",
                         |      "error": "Expected at least 3 items but found 2"
                         |    }
                         |  ]
                         |}
                         |""".stripMargin
}
