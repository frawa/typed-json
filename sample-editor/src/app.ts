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

/// <reference path="./typedjson.d.ts"/>
import { TypedJson, TypedJsonFactory } from "typedjson"
import { EditorSelection, StateEffect, StateEffectType, StateField } from "@codemirror/next/state"

// see https://codemirror.net/6/docs/ref/

const initialJson = `{
  "hello": "world"
}`
const initialSchema = `{
  "type": "boolean"
}`

const schemaUpdate: StateEffectType<TypedJson> = StateEffect.define()

const typedJsonSchemaField: StateField<TypedJson> = StateField.define({
    create(state) {
        return TypedJsonFactory
            .withMetaSchema()
            .forValue(state.doc.sliceString(0))
    },
    update(value, tr) {
        if (tr.docChanged) {
            value = value.forValue(tr.state.doc.sliceString(0))
        }
        return value
    }
})

const typedJsonField: StateField<TypedJson> = StateField.define({
    create(state) {
        return TypedJsonFactory
            .create()
            .forValue(state.doc.sliceString(0))
    },
    update(value, tr) {
        const schema = tr.effects.find(e => e.is(schemaUpdate))?.value
        if (schema) {
            value = value.withSchema(schema)
        }
        if (tr.docChanged) {
            value = value.forValue(tr.state.doc.sliceString(0))
        }
        return value
    }
})

const state = EditorState.create({
    doc: initialJson,
    extensions: [
        basicSetup,
        json(),
        bracketMatching(),
        closeBrackets(),
        typedJsonField,
        autocompletion(autocompleteConfig(typedJsonField)),
        linter(linterFun(typedJsonField)),
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
        typedJsonSchemaField,
        EditorView.updateListener.of(update => {
            if (update.docChanged) {
                // view.dispatch({
                //     effects: [schemaUpdate.of(update.state.field(typedJsonSchemaField))]
                // })
                view.setState(view.state.update({
                    effects: [schemaUpdate.of(update.state.field(typedJsonSchemaField))]
                }).state)
            }
        }),
        autocompletion(autocompleteConfig(typedJsonSchemaField)),
        linter(linterFun(typedJsonSchemaField)),
        // lintGutter(),
        keymap.of(lintKeymap)
        // keymap.of(completionKeymap)
    ],
});

function autocompleteConfig(field: StateField<TypedJson>): CompletionConfig {
    return {
        activateOnTyping: false,
        defaultKeymap: true,
        override: [context => {
            return new Promise(resolve => {
                const typedJson = context.state.field(field)
                const suggestions = typedJson.suggestAt(context.pos)
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

function linterFun(field: StateField<TypedJson>) {
    return (view: EditorView) => {
        const typedJson = view.state.field(field)
        const markers = typedJson.markers().sort((a, b) => a.start - b.start)
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

const view = new EditorView({
    state,
    parent: document.querySelector("#editor") || undefined
});

const viewSchema = new EditorView({
    state: stateSchema,
    parent: document.querySelector("#editorSchema") || undefined
});

view.setState(view.state.update({
    effects: [schemaUpdate.of(viewSchema.state.field(typedJsonSchemaField))]
}).state);

// eslint-disable-next-line @typescript-eslint/no-explicit-any
(window as any).view = view;

// eslint-disable-next-line @typescript-eslint/no-explicit-any
(window as any).viewSchema = viewSchema
