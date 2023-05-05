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

    val typedJson   = TypedJson.create(Sample.schema).toOption.get
    val (output, _) = typedJson.eval(parseJsonValue(Sample.value))
    val basic       = output.map(OutputJson.basic)

    val expected = parser.parse(Sample.expectedBasic).toOption
    assertEquals(basic, expected)
  }

  private def prettyPrint(v: Value): String =
    v match {
      case NullValue         => "null"
      case StringValue(v)    => s"'${v}'"
      case NumberValue(v)    => s"${v}"
      case BoolValue(v)      => s"${v}"
      case ArrayValue(items) => s"[${items.map(prettyPrint).mkString("\n\t,")}\n]"
      case ObjectValue(properties) =>
        s"{${properties.toSeq.sortBy(_._1).map { (p, v) => s"${p}:${prettyPrint(v)}" }.mkString("\n\t,")}\n}"
    }

  test("first basic sample") {
    import BasicOutput.given

    given parser: Parser = new JawnParser()

    val typedJson   = TypedJson.create(Sample.schema).toOption.get
    val (output, _) = typedJson.eval(parseJsonValue(Sample.value))

    // println(s"FW ${munitPrint(output)}")
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
            FalseSchemaReason(), // TODO additional property?
            Pointer.empty / 1 / "z",
            KeywordLocation(
              "/items/$ref/additionalProperties",
              "https://example.com/polygon#/$defs/point/additionalProperties"
            )
          ),
          BasicOutput.Error(
            MissingRequiredProperties(Seq("y")),
            Pointer.empty / 1,
            KeywordLocation("/items/$ref/required", "https://example.com/polygon#/$defs/point/required")
          ),
          BasicOutput.Error(
            MinItemsMismatch(3),
            Pointer.empty,
            KeywordLocation("/minItems")
          )
        ),
        annotations = Seq()
      )
    assertEquals(Some(expected), output)

    // TODO

    // val basicJson            = output.map(OutputJson.basic)
    // val expectedJson         = parser.parse(Sample.expectedBasic).toOption
    // val basicJsonString      = basicJson.map(prettyPrint)
    // val expectetedJsonString = expectedJson.map(prettyPrint)

    // // assertEquals(basicJson, expectedJson)

    // // println(s"FW ${munitPrint(basicJsonString)}")
    // assertEquals(basicJsonString, expectetedJsonString)
  }

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
                                 |      "error": "Expected at least 3 items but found 2"
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
