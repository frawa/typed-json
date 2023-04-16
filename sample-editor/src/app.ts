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

/// <reference path="./typedjson.d.ts"/>
import { TypedJson, TypedJsonFactory } from "typedjson"

import { editor, languages } from 'monaco-editor';
import './app.css';

// @ts-ignore
self.MonacoEnvironment = {
  getWorkerUrl: function (moduleId, label) {
    if (label === 'json') {
      return './json.worker.bundle.js';
    }
    return './editor.worker.bundle.js';
  }
};

const initialJson = `{
  "hello": "world"
}`
const initialSchema = `{
  "type": "boolean"
}`

const common: editor.IEditorOptions = {
  minimap: {
    enabled: false
  },
  bracketPairColorization: {
    enabled: true,
  },
  suggest: {
    preview: true,
    previewMode: 'subwordSmart'
  }
}

const editorJson = editor.create(document.getElementById('editor')!, {
  value: initialJson,
  language: 'json',
  ...common,
  theme: "vs-dark",
});

const editorSchema = editor.create(document.getElementById('editorSchema')!, {
  value: initialSchema,
  language: 'json',
  ...common,
  theme: "vs-dark",
});

// WTF?
(editorJson.getContribution("editor.contrib.suggestController") as any).widget.value._setDetailsVisible(true);
(editorSchema.getContribution("editor.contrib.suggestController") as any).widget.value._setDetailsVisible(true);

let typedJsonSchema = TypedJsonFactory
  .withMetaSchema()
  .forValue(editorSchema.getValue())

let typedJson = TypedJsonFactory
  .create()
  .withSchema(typedJsonSchema)
  .forValue(editorSchema.getValue())

validateAndSetMarkers(typedJsonSchema, editorSchema)
validateAndSetMarkers(typedJson, editorJson)

editorSchema.onDidChangeModelContent(e => {
  typedJsonSchema = typedJsonSchema.forValue(editorSchema.getValue())
  const valid = validateAndSetMarkers(typedJsonSchema, editorSchema)
  if (valid) {
    typedJson = typedJson.withSchema(typedJsonSchema)
  }
})

editorJson.onDidChangeModelContent(e => {
  typedJson = typedJson.forValue(editorJson.getValue())
  validateAndSetMarkers(typedJson, editorJson)
})

const typedJsonById: { [key: string]: () => TypedJson } = {}
typedJsonById[editorJson.getModel()!.id] = () => typedJson
typedJsonById[editorSchema.getModel()!.id] = () => typedJsonSchema

languages.registerCompletionItemProvider("json", {
  triggerCharacters: [' '],
  provideCompletionItems: (model, position, context) => {
    // console.log("FW", model.id, model.uri)
    const typedJsonSuggetions = typedJsonById[model.id]().suggestAt(model.getOffsetAt(position))
    if (typedJsonSuggetions.length === 0) {
      return {
        suggestions: []
      }
    }
    const theSuggestion = typedJsonSuggetions[0];
    // TODO
    const start = model.getPositionAt(theSuggestion.start)
    const end = model.getPositionAt(theSuggestion.end + 1)
    // console.log("FW suggestions", typedJsonSuggetions.length, theSuggestion.suggestions)
    const suggestions: languages.CompletionItem[] = theSuggestion.suggestions.map(s => {
      const value = s.value
      const label = JSON.stringify(value).slice(0, 21)
      const pretty = JSON.stringify(value, null, 2)
      const detail = `${theSuggestion.pointer} ${start.lineNumber}:${start.column}-${end.lineNumber}:${end.column}`
      return {
        label,
        kind: 0,
        detail,
        documentation: {
          value: "```\n" + pretty + "\n```",
        },
        insertText: '',
        range: {
          startColumn: position.column,
          startLineNumber: position.lineNumber,
          endColumn: position.column,
          endLineNumber: position.lineNumber,
        },
        // TODO aggressive replace from typedJson suggestion        
        additionalTextEdits: [{
          //   range: {
          //     startColumn: model.getWordAtPosition(position)?.startColumn ?? position.column,
          //     startLineNumber: position.lineNumber,
          //     endColumn: position.column,
          //     endLineNumber: position.lineNumber
          //   },
          //   text: pretty
          // }, {
          range: {
            startColumn: start.column,
            startLineNumber: start.lineNumber,
            endColumn: end.column,
            endLineNumber: end.lineNumber
          },
          text: pretty
        }]
      };
    });
    return {
      suggestions
    }
  }
})

function validateAndSetMarkers(tj: TypedJson, e: editor.IStandaloneCodeEditor): boolean {
  const model = e.getModel()
  if (!model) {
    return false;
  }
  const markers: editor.IMarkerData[] = tj.markers().map(marker => {
    const start = model.getPositionAt(marker.start)
    const end = model.getPositionAt(marker.end)
    return <editor.IMarkerData>{
      severity: 8, //editor.MarkerSeverity.Error,
      message: marker.message,
      source: marker.pointer,
      startLineNumber: start.lineNumber,
      startColumn: start.column,
      endLineNumber: end.lineNumber,
      endColumn: end.column
    }
  })
  editor.setModelMarkers(model, "TypedJson", markers)
  return markers.length === 0;
}

// languages.json.jsonDefaults.setDiagnosticsOptions({})


function replaceSchemaBy(value: string) {
  editorSchema.setValue(value)
}

document.querySelector<HTMLSelectElement>("#sample-schema")?.addEventListener("change", (e) => {
  const value: string = (e.target as any)?.value ?? ''
  switch (value) {
    case 'properties':
      replaceSchemaBy(`{
        "$schema": "https://json-schema.org/draft/2020-12/schema",
          "properties": {
          "foo": { "type": "array", "maxItems": 3 },
          "bar": { "type": "array" }
        },
        "patternProperties": { "f.o": { "minItems": 2 } },
        "additionalProperties": { "type": "integer" }
      } `)
      break;
    case 'if-then-else':
      replaceSchemaBy(`{
        "$schema": "https://json-schema.org/draft/2020-12/schema",
          "then": { "const": "yes" },
        "else": { "const": "other" },
        "if": { "maxLength": 4 }
      } `)
      break;
    case 'all-of':
      replaceSchemaBy(`{
        "$schema": "https://json-schema.org/draft/2020-12/schema",
          "properties": { "bar": { "type": "integer" } },
        "required": ["bar"],
          "allOf" : [
            {
              "properties": {
                "foo": { "type": "string" }
              },
              "required": ["foo"]
            },
            {
              "properties": {
                "baz": { "type": "null" }
              },
              "required": ["baz"]
            }
          ]
      } `)
  }
});
