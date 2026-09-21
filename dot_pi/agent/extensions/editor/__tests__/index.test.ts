import { readFile } from "node:fs/promises";
import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { KeybindingsManager, setKeybindings, TUI_KEYBINDINGS } from "@earendil-works/pi-tui";
import { expect, test } from "vitest";

import configureEditor from "../index.ts";

const keybindings = JSON.parse(
  await readFile(new URL("../../../keybindings.json", import.meta.url), "utf8"),
);

function createEditor() {
  let onSessionStart: ((event: unknown, ctx: any) => void) | undefined;

  configureEditor({
    on(event: string, handler: (event: unknown, ctx: any) => void) {
      expect(event).toBe("session_start");
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

  if (!editorFactory) throw new Error("Editor component was not registered");
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

  expect(editor.getPaddingX()).toBe(1);
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

    expect(submitted).toBe(command);
    expect(editor.getText()).toBe("");
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

  expect(submitted).toBeUndefined();
  expect(editor.getText()).toBe("/reload now\n");
});

test("submit is bound to Ctrl+Enter but not Super+Enter", () => {
  expect(keybindings["tui.input.newLine"]).toEqual(["enter", "shift+enter", "ctrl+j"]);
  expect(keybindings["tui.input.submit"]).toEqual(["ctrl+enter"]);
});
