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

package frawa.typedjson.testutil

import frawa.typedjson.keywords.SchemaValue
import frawa.typedjson.parser.Value.*

object TestSchemas:
  val nullSchema        = """{"type": "null"}"""
  val boolSchema        = """{"type": "boolean"}"""
  val trueSchema        = """true"""
  val falseSchema       = """false"""
  val notFalseSchema    = """{"not": false}"""
  val emtpySchema       = """{}"""
  val stringSchema      = """{"type": "string"}"""
  val numberSchema      = """{"type": "number"}"""
  val arraySchema       = """{"type": "array"}"""
  val numberArraySchema = """{"type": "array", "items": { "type": "number"}}"""

  val totoObjectSchema: String = """{
                                   |"type": "object",
                                   |"properties": {
                                   |  "toto": { "type": "number" },
                                   |  "titi": { "type": "string" }
                                   |}
                                   |}
                                   |""".stripMargin

  val totoRequiredObjectSchema: String = """{
                                           |"type": "object",
                                           |"required": ["toto", "gnu"],
                                           |"properties": {
                                           |  "toto": { "type": "number" },
                                           |  "gnu": { "type": "boolean" },
                                           |  "titi": { "type": "string" }
                                           |}
                                           |}
                                           |""".stripMargin

  val allOfSchema: String = """{
                              |"allOf": [
                              |  { "type": "number" }
                              |]
                              |}
                              |""".stripMargin

  val anyOfSchema: String = """{
                              |"anyOf": [
                              |  { "type": "number" },
                              |  { "type": "string" }
                              |]
                              |}
                              |""".stripMargin

  val oneOfSchema: String = """{
                              |"oneOf": [
                              |  { "type": "number" },
                              |  { "type": "string" }
                              |]
                              |}
                              |""".stripMargin

  val ifThenElseSchema: String = """{
                                   |"if": { "type": "number" },
                                   |"then": { "type": "number" },
                                   |"else": { "type": "string" }
                                   |}
                                   |""".stripMargin

  val nullOrStringSchema = """{"type": ["null","string"]}"""

  val enumSchema: String = """{
                             |"type": "string",
                             |"enum": ["foo", "bar"]
                             |}""".stripMargin
  val constSchema: String = """{
                              |"type": "string",
                              |"const": "first"
                              |}""".stripMargin

  val idRefDefsSchema: String = """{
                                  |"$id": "https://example.net/root.json",
                                  |"type": "array",
                                  |"items": {
                                  |    "$ref": "#item"
                                  |},
                                  |"$defs": {
                                  |    "single": {
                                  |        "$anchor": "item",
                                  |        "type": "number"
                                  |    }
                                  |}
                                  |}""".stripMargin

  val missingIdRefDefsSchema: String = """{
                                         |"$id": "https://example.net/root.json",
                                         |"type": "array",
                                         |"items": {
                                         |    "$ref": "#missing"
                                         |}
                                         |}""".stripMargin

  val recursiveRefDefsSchema: String = """|{
                                          |"$ref": "#/$defs/list",
                                          |"$defs": {
                                          |  "list": {
                                          |    "oneOf": [
                                          |      { "$ref": "#/$defs/list" },
                                          |      { "type": "string" }
                                          |    ]
                                          |  }
                                          |}
                                          |}
                                          |""".stripMargin

  val subItemRefDefsSchema: String = """|{"$defs": {
                                        |"item": {
                                        |  "type": "array",
                                        |  "items": false,
                                        |  "prefixItems": [
                                        |    { "$ref": "#/$defs/sub-item" },
                                        |    { "$ref": "#/$defs/sub-item" }
                                        |  ]
                                        |},
                                        |"sub-item": {
                                        |  "type": "object",
                                        |  "required": ["foo"]
                                        |}
                                        |},
                                        |"type": "array",
                                        |"items": false,
                                        |"prefixItems": [
                                        |  { "$ref": "#/$defs/item" },
                                        |  { "$ref": "#/$defs/item" },
                                        |  { "$ref": "#/$defs/item" }
                                        |]
                                        |}""".stripMargin

  val numberSchemaValue: SchemaValue = SchemaValue(
    value = ObjectValue(
      properties = Map(
        "type" -> StringValue(
          value = "number"
        )
      )
    )
  )

  val stringSchemaValue: SchemaValue = SchemaValue(
    value = ObjectValue(
      properties = Map(
        "type" -> StringValue(
          value = "string"
        )
      )
    )
  )

  val refInPropertiesSchema: String = """{
                                        |"$id": "https://example.net/root.json",
                                        |"type": "object",
                                        |"properties": {
                                        |    "foo": { "$ref": "#foo" }
                                        |},
                                        |"$defs": {
                                        |    "foo": {
                                        |        "$anchor": "foo",
                                        |        "type": "number"
                                        |    }
                                        |}
                                        |}""".stripMargin

  val refAtRootSchema: String = """{
                                  |"$id": "https://example.net/root.json",
                                  |"$ref": "#object",
                                  |"$defs": {
                                  |    "object": {
                                  |      "$anchor": "object",
                                  |      "type": "object",
                                  |      "properties": {
                                  |         "foo": {
                                  |           "anyOf": [
                                  |             { "$ref": "#/$defs/numberType" },
                                  |             {
                                  |               "type": "array",
                                  |               "items": { "$ref": "#/$defs/numberType" },
                                  |               "minItems": 1,
                                  |               "uniqueItems": true
                                  |             }
                                  |           ]
                                  |         }
                                  |      }  
                                  |    },
                                  |    "numberType": {
                                  |       "type": "number"
                                  |    }
                                  |}
                                  |}""".stripMargin

  val refToValidationSpec: String = """{
                                      |"$id": "https://json-schema.org/draft/2020-12/partial",
                                      |"$dynamicAnchor": "meta",
                                      |"allOf": [
                                      |  {"$ref": "https://json-schema.org/draft/2020-12/meta/validation"},
                                      |  {"$ref": "https://json-schema.org/draft/2020-12/meta/core"}
                                      |]
                                      |}""".stripMargin

  val refIndirectToValidationSpec: String = """{
                                              |"$id": "https://example.net/root.json",
                                              |"$ref": "https://json-schema.org/draft/2020-12/schema"
                                              |}""".stripMargin
