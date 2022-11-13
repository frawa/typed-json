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

import { autocompletion, Completion, CompletionResult } from "@codemirror/next/autocomplete"
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
        autocompletion(autocompleteConfig(typedJsonSchemaField)),
        linter(linterFun(typedJsonSchemaField)),
        // lintGutter(),
        keymap.of(lintKeymap),
        // keymap.of(completionKeymap)
        EditorView.updateListener.of(update => {
            if (update.docChanged) {
                const pos = view.state.selection.mainIndex
                const transaction = view.state.update({
                    effects: [schemaUpdate.of(update.state.field(typedJsonSchemaField))],
                    changes: [
                        // noop triggers linter, but keeps caret
                        { from: pos, to: pos + 1, insert: view.state.doc.slice(pos, pos + 1) },
                    ]
                })
                view.dispatch(transaction)
            }
        })
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
                const theStart = theSuggestion.start
                const theEnd = theSuggestion.end
                const options = theSuggestion.suggestions.map(suggestion => {
                    const value = suggestion.value
                    const label = JSON.stringify(value).slice(0, 21)
                    const pretty = JSON.stringify(value, null, 2)
                    return ({
                        label,
                        info: pretty,
                        apply: (view: EditorView, completion: Completion, from1: number, to1: number) => {
                            const insert = completion.info as string
                            const from = theStart !== 0 ? theStart : from1
                            const to = theStart !== 0 ? theEnd : to1
                            const replace = view.state.update({
                                changes: [
                                    { from, to, insert }
                                ],
                                selection: EditorSelection.cursor(from + (insert?.length ?? 0))
                            });
                            view.update([replace])
                        }
                    });
                }).slice(0, 42)
                return resolve({
                    from: theStart !== 0 ? theStart : context.pos,
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

function replaceSchemaBy(value: string) {
    const transaction = viewSchema.state.update({
        changes: { from: 0, to: viewSchema.state.doc.length, insert: value }
    })
    viewSchema.dispatch(transaction)
}

document.querySelector<HTMLSelectElement>("#sample-schema")?.addEventListener("change", (e) => {
    const value: string = (e.target as any)?.value ?? ''
    switch (value) {
        case 'properties':
            replaceSchemaBy(`{
    "$schema": "https://json-schema.org/draft/2020-12/schema",
    "properties": {
    "foo": {"type": "array", "maxItems": 3},
        "bar": {"type": "array"}
    },
    "patternProperties": {"f.o": {"minItems": 2}},
    "additionalProperties": {"type": "integer"}
}`)
            break;
        case 'if-then-else':
            replaceSchemaBy(`{
    "$schema": "https://json-schema.org/draft/2020-12/schema",
    "then": { "const": "yes" },
    "else": { "const": "other" },
    "if": { "maxLength": 4 }
}`)
            break;
        case 'all-of':
            replaceSchemaBy(`{
            "$schema": "https://json-schema.org/draft/2020-12/schema",
            "properties": {"bar": {"type": "integer"}},
            "required": ["bar"],
            "allOf" : [
                {
                    "properties": {
                        "foo": {"type": "string"}
                    },
                    "required": ["foo"]
                },
                {
                    "properties": {
                        "baz": {"type": "null"}
                    },
                    "required": ["baz"]
                }
            ]
        }`)
    }
});

// eslint-disable-next-line @typescript-eslint/no-explicit-any
(window as any).view = view;

// eslint-disable-next-line @typescript-eslint/no-explicit-any
(window as any).viewSchema = viewSchema
