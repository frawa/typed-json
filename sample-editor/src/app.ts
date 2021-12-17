import { EditorState, EditorView, basicSetup } from "@codemirror/next/basic-setup"
import { json } from "@codemirror/next/lang-json"
import { autocompletion } from "@codemirror/next/autocomplete"

/// <reference path="./typedjson.d.ts"/>
import { TmpMain } from "typedjson"
TmpMain.hello()

// see https://codemirror.net/6/docs/ref/


const state = EditorState.create({
    doc: `{
  "hello": "world"
}`,
    extensions: [
        basicSetup,
        json(),
        autocompletion({
            activateOnTyping: false,
            defaultKeymap: true,
            override: [context => {
                return {
                    from: context.pos,
                    options: [{
                        label: "a suggestion"
                    }]
                }
            }]
        }),
        // keymap.of(completionKeymap)
    ],
});


// eslint-disable-next-line @typescript-eslint/no-explicit-any
(window as any).view =
    new EditorView({
        state,
        parent: document.querySelector("#editor") || undefined
    })
