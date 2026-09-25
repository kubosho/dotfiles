import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import {
  KeybindingsManager,
  TUI_KEYBINDINGS,
  stripTerminalSequences,
  visibleWidth,
} from "@earendil-works/pi-tui";
import { expect, test } from "vitest";

import configureEditor from "../index.ts";

function createEditor() {
  let onSessionStart: ((event: unknown, ctx: any) => void) | undefined;
  const theme = {
    fg: (_color: string, text: string) => text,
    borderColor: (text: string) => text,
    selectList: {},
  };
  const ui = {
    theme,
    setEditorComponent(factory: typeof editorFactory) {
      editorFactory = factory;
    },
  };
  let editorFactory: ((tui: any, theme: any, keybindings: any) => any) | undefined;

  configureEditor({
    on(event: string, handler: (event: unknown, ctx: any) => void) {
      expect(event).toBe("session_start");
      onSessionStart = handler;
    },
    registerCommand() {
      throw new Error("The editor must not register a style command");
    },
  } as ExtensionAPI);

  onSessionStart?.({}, { mode: "tui", ui });

  if (!editorFactory) throw new Error("Editor was not registered");
  return editorFactory(
    { terminal: { rows: 24 }, requestRender() {} },
    theme,
    new KeybindingsManager(TUI_KEYBINDINGS),
  );
}

test("the main editor reserves space for the border, prompt, and padding", () => {
  const editor = createEditor();

  editor.setPaddingX(0);

  expect(editor.getPaddingX()).toBe(3);
});

test("the frame places the ornament on the right without changing the draft", () => {
  const editor = createEditor();
  editor.setText("draft");

  const lines = editor.render(16);

  expect(lines[0]).toBe("┏━━━━━━━━━━━ ✦ ┓");
  expect(stripTerminalSequences(lines[1])).toContain("┃❯ draft");
  expect(lines[2]).toBe("┗━━━━━━━━━━━━━━┛");
  expect(editor.getText()).toBe("draft");
});

test("only the first line displays the prompt", () => {
  const editor = createEditor();
  editor.setText("first\nsecond");

  const lines = editor.render(16).map(stripTerminalSequences);

  expect(lines[1]).toContain("┃❯ first");
  expect(lines[2]).toContain("┃  second");
  expect(editor.getText()).toBe("first\nsecond");
});

test("an empty editor displays the prompt before the cursor", () => {
  const editor = createEditor();

  expect(stripTerminalSequences(editor.render(16)[1])).toContain("┃❯  ");
  expect(editor.getText()).toBe("");
});

test("long drafts retain scroll indicators in the frame", () => {
  const editor = createEditor();
  editor.setText(Array.from({ length: 12 }, (_, index) => `line ${index}`).join("\n"));

  const lines = editor.render(20);

  expect(lines[0]).toContain("↑");
  expect(lines[1]).not.toContain("❯");
  expect(lines.at(-1)).toBe("┗━━━━━━━━━━━━━━━━━━┛");
});

test("a narrow terminal still renders a line within its width", () => {
  const editor = createEditor();

  for (const width of [1, 2, 3, 4, 5, 6, 7]) {
    expect(editor.render(width).every((line: string) => visibleWidth(line) <= width)).toBe(true);
  }
});
