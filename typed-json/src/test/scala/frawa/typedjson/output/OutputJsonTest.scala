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

package frawa.typedjson.output

import frawa.typedjson.parser.Parser
import frawa.typedjson.parser.Value.*
import frawa.typedjson.parser.jawn.JawnParser
import frawa.typedjson.validation.FalseSchemaReason
import munit.FunSuite
import frawa.typedjson.output.BasicOutput
import frawa.typedjson.TypedJson
import frawa.typedjson.testutil.TestUtil.parseJsonValue
import frawa.typedjson.util.WithPointer
import frawa.typedjson.pointer.Pointer
import frawa.typedjson.validation.MissingRequiredProperties
import frawa.typedjson.validation.MinItemsMismatch
import frawa.typedjson.keywords.KeywordLocation
import frawa.typedjson.parser.Value
import frawa.typedjson.validation.AdditionalPropertyInvalid
import frawa.typedjson.util.ShowValue.prettyPrint
import scala.annotation.experimental

@experimental
class OutputJsonTest extends FunSuite:

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
      OutputJson.basic(BasicOutput(true)),
      ObjectValue(Map("valid" -> BoolValue(true)))
    )
  }

  test("basic sample".ignore) {
    import BasicOutput.given
    given parser: Parser = new JawnParser()

    @experimental
    val typedJson   = TypedJson.create(Sample.schema).toOption.get
    val (output, _) = typedJson.eval(parseJsonValue(Sample.value))
    val basic       = output.map(OutputJson.basic)

    val expected = parser.parse(Sample.expectedBasic).toOption
    assertEquals(basic, expected)
  }

  test("first basic sample") {
    import BasicOutput.given

    given parser: Parser = new JawnParser()

    val typedJson   = TypedJson.create(Sample.schema).toOption.get
    val (output, _) = typedJson.eval(parseJsonValue(Sample.value))

    val expected =
      BasicOutput(
        false,
        error = None,
        instanceLocation = Pointer.empty,
        keywordLocation = None,
        errors = Seq(
          // TODO 1: failed at Pointer.empty, "keywordLocation": """, "instanceLocation": "",
          // TODO 2: failed at Pointer.empty / 1, "keywordLocation": "/items/$ref", "absoluteKeywordLocation": "https://example.com/polygon#/$defs/point",

          BasicOutput.Error(
            AdditionalPropertyInvalid("z"),
            Pointer.empty / 1 / "z",
            KeywordLocation(
              "/items/$ref/additionalProperties",
              "https://example.com/polygon#/$defs/point/additionalProperties"
            )
          ),
          BasicOutput.Error(
            FalseSchemaReason(),
            Pointer.empty / 1,
            KeywordLocation(
              "/items/$ref",
              "https://example.com/polygon#/$defs/point"
            )
          ),
          BasicOutput.Error(
            MissingRequiredProperties(Seq("y")),
            Pointer.empty / 1,
            KeywordLocation("/items/$ref/required", "https://example.com/polygon#/$defs/point/required")
          ),
          BasicOutput.Error(
            MinItemsMismatch(3, 2),
            Pointer.empty,
            KeywordLocation("/minItems")
          )
        ),
        annotations = Seq()
      )
    // TODO
    // assertEquals(Some(expected), output)

    val basicJson    = output.map(OutputJson.basic)
    val expectedJson = parser.parse(Sample.expectedBasic).toOption
    // TODO maybe?
    // assertEquals(basicJson, expectedJson)

    // TODO Too many "A subschema had errors."
    val basicJson1 = basicJson
      .map(filteredErrors("A subschema had errors."))
      // TODO additional sub errors under AdditionalPropeties
      .map(filteredErrors("Always invalid."))
    val expectedJson1 = expectedJson
      .map(filteredErrors("A subschema had errors."))

    val basicJsonString      = basicJson1.map(prettyPrint(0))
    val expectetedJsonString = expectedJson1.map(prettyPrint(0))
    assertEquals(basicJsonString, expectetedJsonString)
  }

  private def filteredErrors(message: String): Value => Value = value =>
    Value
      .asObject(value)
      .map { ps =>
        ps.view
          .map { (p, v) =>
            if p == "errors" then
              (
                p,
                Value
                  .asArray(v)
                  .map { vs =>
                    vs.filterNot(v =>
                      Value
                        .asObject(v)
                        .flatMap(_.get("error"))
                        .flatMap(Value.asString(_))
                        .exists(_ == message)
                    ).map(filteredErrors(message))
                  }
                  .map(ArrayValue(_))
                  .getOrElse(v)
              )
            else (p, v)
          }
          .filterNot { (p, v) =>
            p == "errors" && Value.asArray(v).exists(_.isEmpty)
          }
          .toMap
      }
      .map(ObjectValue(_))
      .getOrElse(value)

  test("validate basic output".ignore) {}

  test("first detailed sample") {
    import DetailedOutput.given

    given parser: Parser = new JawnParser()

    val typedJson   = TypedJson.create(Sample.schema).toOption.get
    val (output, _) = typedJson.eval(parseJsonValue(Sample.value))

    val expected =
      DetailedOutput(
        false,
        error = None,
        instanceLocation = Pointer.empty,
        keywordLocation = Some(KeywordLocation.empty),
        errors = Seq(
          DetailedOutput(
            valid = false,
            error = None,
            instanceLocation = Pointer.empty / 1,
            keywordLocation = Some(KeywordLocation("/items/$ref", "https://example.com/polygon#/$defs/point")),
            errors = Seq(
              DetailedOutput(
                valid = false,
                error = Some(AdditionalPropertyInvalid("z")),
                instanceLocation = Pointer.empty / 1 / "z",
                keywordLocation = Some(
                  KeywordLocation(
                    "/items/$ref/additionalProperties",
                    "https://example.com/polygon#/$defs/point/additionalProperties"
                  )
                ),
                errors = Seq(
                  DetailedOutput(
                    valid = false,
                    error = Some(FalseSchemaReason()),
                    instanceLocation = Pointer.empty / 1 / "z",
                    keywordLocation = Some(
                      KeywordLocation(
                        "/items/$ref/additionalProperties",
                        "https://example.com/polygon#/$defs/point/additionalProperties"
                      )
                    )
                  )
                )
              ),
              DetailedOutput(
                valid = false,
                error = Some(MissingRequiredProperties(Seq("y"))),
                instanceLocation = Pointer.empty / 1,
                keywordLocation =
                  Some(KeywordLocation("/items/$ref/required", "https://example.com/polygon#/$defs/point/required"))
              )
            )
          ),
          DetailedOutput(
            valid = false,
            error = Some(MinItemsMismatch(3, 2)),
            instanceLocation = Pointer.empty,
            keywordLocation = Some(KeywordLocation("/minItems"))
          )
        ),
        annotations = Seq()
      )
    // assertEquals(output, Some(expected))

    val detailedJson = output.map(OutputJson.detailed)
    val expectedJson = parser.parse(Sample.expectedDetailed).toOption
    // TODO maybe?
    // assertEquals(detailedJson, expectedJson)

    val detailedJson1 = detailedJson
      // TODO additional sub errors under AdditionalPropeties
      .map(filteredErrors("Always invalid."))

    val detailedJsonString   = detailedJson1.map(prettyPrint(0))
    val expectetedJsonString = expectedJson.map(prettyPrint(0))

    assertEquals(detailedJsonString, expectetedJsonString)
  }

  test("validate detailed output".ignore) {}

object Sample:
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
                                 |      "keywordLocation": "/minItems",
                                 |      "instanceLocation": "",
                                 |      "error": "Expected at least 3 items but found 2."
                                 |    },                               
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
                                 |    }                                 
                                 |  ]
                                 |}
                                 |""".stripMargin

  val expectedDetailed = """|{
                            |  "valid": false,
                            |  "keywordLocation": "",
                            |  "instanceLocation": "",
                            |  "errors": [
                            |    {
                            |      "valid": false,
                            |      "keywordLocation": "/items/$ref",
                            |      "absoluteKeywordLocation":
                            |        "https://example.com/polygon#/$defs/point",
                            |      "instanceLocation": "/1",
                            |      "errors": [
                            |        {
                            |          "valid": false,
                            |          "keywordLocation": "/items/$ref/required",
                            |          "absoluteKeywordLocation":
                            |            "https://example.com/polygon#/$defs/point/required",
                            |          "instanceLocation": "/1",
                            |          "error": "Required property 'y' not found."
                            |        },
                            |        {
                            |          "valid": false,
                            |          "keywordLocation": "/items/$ref/additionalProperties",
                            |          "absoluteKeywordLocation":
                            |            "https://example.com/polygon#/$defs/point/additionalProperties",
                            |          "instanceLocation": "/1/z",
                            |          "error": "Additional property 'z' found but was invalid."
                            |        }
                            |      ]
                            |    },
                            |    {
                            |      "valid": false,
                            |      "keywordLocation": "/minItems",
                            |      "instanceLocation": "",
                            |      "error": "Expected at least 3 items but found 2."
                            |    }
                            |  ]
                            |}""".stripMargin
