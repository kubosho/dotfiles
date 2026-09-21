import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import test from "node:test";
import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { KeybindingsManager, setKeybindings, TUI_KEYBINDINGS } from "@earendil-works/pi-tui";

import configureEditor from "../index.ts";

const keybindings = JSON.parse(
  await readFile(new URL("../../../keybindings.json", import.meta.url), "utf8"),
);

function createEditor() {
  let onSessionStart: ((event: unknown, ctx: any) => void) | undefined;

  configureEditor({
    on(event: string, handler: (event: unknown, ctx: any) => void) {
      assert.equal(event, "session_start");
      onSessionStart = handler;
    },
  } as ExtensionAPI);

  let editorFactory: ((tui: any, theme: any, keybindings: any) => any) | undefined;
  onSessionStart?.(
    {},
    {
      mode: "tui",
      ui: {
        setEditorComponent(factory: typeof editorFactory) {
          editorFactory = factory;
        },
      },
    },
  );

  assert.ok(editorFactory);
  const keybindingsManager = new KeybindingsManager(TUI_KEYBINDINGS, keybindings);
  setKeybindings(keybindingsManager);

  return editorFactory(
    { requestRender() {} },
    { borderColor: (text: string) => text, selectList: {} },
    keybindingsManager,
  );
}

test("the main editor keeps one column of horizontal padding", () => {
  const editor = createEditor();

  editor.setPaddingX(0);

  assert.equal(editor.getPaddingX(), 1);
});

test("Enter submits a slash command without arguments", () => {
  for (const command of ["/reload", "/skill:baseline"]) {
    const editor = createEditor();
    let submitted: string | undefined;
    editor.onSubmit = (text: string) => {
      submitted = text;
    };
    editor.setText(command);

    editor.handleInput("\r");

    assert.equal(submitted, command);
    assert.equal(editor.getText(), "");
  }
});

test("Enter inserts a newline when a slash command has arguments", () => {
  const editor = createEditor();
  let submitted: string | undefined;
  editor.onSubmit = (text: string) => {
    submitted = text;
  };
  editor.setText("/reload now");

  editor.handleInput("\r");

  assert.equal(submitted, undefined);
  assert.equal(editor.getText(), "/reload now\n");
});

test("submit is bound to Ctrl+Enter but not Super+Enter", () => {
  assert.deepEqual(keybindings["tui.input.newLine"], ["enter", "shift+enter", "ctrl+j"]);
  assert.deepEqual(keybindings["tui.input.submit"], ["ctrl+enter"]);
});
