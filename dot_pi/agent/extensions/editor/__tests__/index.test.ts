import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { KeybindingsManager, TUI_KEYBINDINGS } from "@earendil-works/pi-tui";
import { expect, test } from "vitest";

import configureEditor from "../index.ts";

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
  const keybindingsManager = new KeybindingsManager(TUI_KEYBINDINGS);

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
