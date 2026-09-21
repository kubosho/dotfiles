import { CustomEditor, type ExtensionAPI } from "@earendil-works/pi-coding-agent";

class PaddedEditor extends CustomEditor {
  override setPaddingX(_padding: number): void {
    super.setPaddingX(1);
  }
}

export default function (pi: ExtensionAPI) {
  pi.on("session_start", (_event, ctx) => {
    if (ctx.mode !== "tui") return;

    ctx.ui.setEditorComponent(
      (tui, theme, keybindings) =>
        new PaddedEditor(tui, theme, keybindings, {
          paddingX: 1,
        }),
    );
  });
}
