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

import { autocompletion } from "@codemirror/next/autocomplete"
import { CompletionConfig } from "@codemirror/next/autocomplete/src/config"
import { basicSetup, EditorState, EditorView } from "@codemirror/next/basic-setup"
import { closeBrackets } from "@codemirror/next/closebrackets"
import { json } from "@codemirror/next/lang-json"
import { Diagnostic, linter, lintKeymap } from "@codemirror/next/lint"
import { bracketMatching } from "@codemirror/next/matchbrackets"
import { keymap } from "@codemirror/next/view"
import { Text } from "@codemirror/next/text"

/// <reference path="./typedjson.d.ts"/>
import { TypedJson, TypedJsonFactory } from "typedjson"
import { EditorSelection } from "@codemirror/next/state"

// see https://codemirror.net/6/docs/ref/

const initialJson = `{
  "hello": "world"
}`
const initialSchema = `{
  "type": "boolean"
}`

let typedJsonSchema = TypedJsonFactory
    .withMetaSchema()
    .forValue(initialSchema)
let typedJson = TypedJsonFactory
    .create()
    .withSchema(typedJsonSchema)
    .forValue(initialJson)

const state = EditorState.create({
    doc: initialJson,
    extensions: [
        basicSetup,
        json(),
        bracketMatching(),
        closeBrackets(),
        EditorView.updateListener.of(update => {
            if (update.docChanged) {
                typedJson = typedJson.forValue(update.state.doc.sliceString(0))
            }
        }),
        autocompletion(autocompleteConfig(() => typedJson)),
        linter(linterFun(() => typedJson)),
        // lintGutter(),
        keymap.of(lintKeymap)
        // keymap.of(completionKeymap)
    ],
});

const stateSchema = EditorState.create({
    doc: initialSchema,
    extensions: [
        basicSetup,
        json(),
        bracketMatching(),
        closeBrackets(),
        EditorView.updateListener.of(update => {
            if (update.docChanged) {
                typedJsonSchema = typedJsonSchema.forValue(update.state.doc.sliceString(0))
                typedJson = typedJson.withSchema(typedJsonSchema)
            }
        }),
        autocompletion(autocompleteConfig(() => typedJsonSchema)),
        linter(linterFun(() => typedJsonSchema)),
        // lintGutter(),
        keymap.of(lintKeymap)
        // keymap.of(completionKeymap)
    ],
});

// eslint-disable-next-line @typescript-eslint/no-explicit-any
(window as any).view =
    new EditorView({
        state,
        parent: document.querySelector("#editor") || undefined
    });

// eslint-disable-next-line @typescript-eslint/no-explicit-any
(window as any).viewSchema =
    new EditorView({
        state: stateSchema,
        parent: document.querySelector("#editorSchema") || undefined
    })

function autocompleteConfig(getTypedJson: () => TypedJson): CompletionConfig {
    return {
        activateOnTyping: false,
        defaultKeymap: true,
        override: [context => {
            return new Promise(resolve => {
                const suggestions = getTypedJson().suggestAt(context.pos)
                if (suggestions.length === 0) {
                    return resolve(null)
                }
                const theSuggestion = suggestions[0]
                const from = theSuggestion.start
                const to = theSuggestion.end
                const options = theSuggestion.suggestions.map(suggestion => {
                    const value = suggestion.value
                    const label = JSON.stringify(value).slice(0, 21)
                    const pretty = JSON.stringify(value, null, 2)
                    return ({
                        label,
                        info: pretty,
                        // apply: pretty
                        apply: (view: EditorView) => {
                            const replace = view.state.update({
                                changes: [
                                    { from, to, insert: pretty }
                                ],
                                selection: EditorSelection.cursor(from + pretty.length)
                            });
                            view.update([replace])
                        }
                    });
                });
                // console.log("FW", theSuggestion.suggestions.length, options.length)
                console.log("FW", context.pos, from, to)
                return resolve({
                    // from: context.pos,
                    from,
                    to: context.pos,
                    options
                });
            });
        }]
    }
}

function linterFun(getTypedJson: () => TypedJson) {
    return (view: EditorView) => {
        const markers = getTypedJson().markers()
        const diagnostics: Diagnostic[] = markers.map(marker => ({
            from: marker.start,
            to: marker.end - 1,
            message: marker.message,
            severity: "error", // TODO from marker.severity,
            source: marker.pointer
        }));
        return diagnostics;
    }
}