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

// see https://codemirror.net/6/docs/ref/

const initialJson = `{
  "hello": "world"
}`
const initialSchema = `{
  "type": "boolean"
}`

let typedJsonSchema = TypedJsonFactory
    .withMetaSchema()
// .forValue(initialJson)
let typedJson = TypedJsonFactory
    .create()
// .withSchema(typedJsonSchema)
// .forValue(initialJson)

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
            const suggestions = getTypedJson().suggestAt(context.pos)
            const options = suggestions.map(item => ({
                label: item
            }))
            return {
                from: context.pos,
                options
            }
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