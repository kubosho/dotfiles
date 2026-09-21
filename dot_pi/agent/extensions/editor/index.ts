import { CustomEditor, type ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { matchesKey } from "@earendil-works/pi-tui";

const CTRL_ENTER_SEQUENCE = "\x1b[13;5u";

class SlashCommandEditor extends CustomEditor {
  override setPaddingX(_padding: number): void {
    super.setPaddingX(1);
  }

  handleInput(data: string): void {
    if (matchesKey(data, "enter") && /^\/\S+$/.test(this.getText().trim())) {
      super.handleInput(CTRL_ENTER_SEQUENCE);
      return;
    }

    super.handleInput(data);
  }
}

export default function (pi: ExtensionAPI) {
  pi.on("session_start", (_event, ctx) => {
    if (ctx.mode !== "tui") return;

    ctx.ui.setEditorComponent(
      (tui, theme, keybindings) =>
        new SlashCommandEditor(tui, theme, keybindings, {
          paddingX: 1,
        }),
    );
  });
}
