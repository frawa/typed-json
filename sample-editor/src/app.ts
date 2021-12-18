import { autocompletion } from "@codemirror/next/autocomplete"
import { basicSetup, EditorState, EditorView } from "@codemirror/next/basic-setup"
import { closeBrackets } from "@codemirror/next/closebrackets"
import { json } from "@codemirror/next/lang-json"
import { Diagnostic, linter } from "@codemirror/next/lint"
import { bracketMatching } from "@codemirror/next/matchbrackets"
import { darkTheme } from "@codemirror/next/view/src/theme"

/// <reference path="./typedjson.d.ts"/>
import { TypedJsonFactory } from "typedjson"

// see https://codemirror.net/6/docs/ref/

let typedJson = TypedJsonFactory.withMetaSchema()

const state = EditorState.create({
    doc: `{
  "hello": "world"
}`,
    extensions: [
        basicSetup,
        json(),
        bracketMatching(),
        closeBrackets(),
        autocompletion({
            activateOnTyping: false,
            defaultKeymap: true,
            override: [context => {
                const suggestions = typedJson.suggestAt(context.pos)
                const options = suggestions.map(item => ({
                    label: item
                }))
                return {
                    from: context.pos,
                    options
                }
            }]
        }),
        linter(view => {
            // TODO update only on doc change
            typedJson = typedJson.forValue(view.state.doc.sliceString(0))

            const markers = typedJson.markers()
            const diagnostics: Diagnostic[] = markers.map(marker => ({
                from: marker.start,
                to: marker.end - 1,
                message: marker.message,
                severity: "error", // TODO from marker.severity,
                source: marker.pointer
            }));
            return diagnostics;
        }),
        // lintGutter(), // TODO
        // keymap.of(lintKeymap)
        // keymap.of(completionKeymap)
    ],
});


// eslint-disable-next-line @typescript-eslint/no-explicit-any
(window as any).view =
    new EditorView({
        state,
        parent: document.querySelector("#editor") || undefined
    })
function lintGutter(): import("@codemirror/next/state").Extension {
    throw new Error("Function not implemented.")
}

