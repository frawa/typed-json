import { EditorState, EditorView, basicSetup } from "@codemirror/next/basic-setup"
import { json } from "@codemirror/next/lang-json"
import { autocompletion } from "@codemirror/next/autocomplete"
import { bracketMatching } from "@codemirror/next/matchbrackets"
import { closeBrackets } from "@codemirror/next/closebrackets"

/// <reference path="./typedjson.d.ts"/>
import { SuggestFactory } from "typedjson"

// see https://codemirror.net/6/docs/ref/

let suggest = SuggestFactory.withMetaSchema()

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
                // TODO update only on doc change
                suggest = suggest.forValue(context.state.doc.sliceString(0))

                const suggestions = suggest.at(context.pos)
                const options = suggestions.map(item => ({
                    label: item
                }))
                return {
                    from: context.pos,
                    options
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
