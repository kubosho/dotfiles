import { CustomEditor, type ExtensionAPI } from "@earendil-works/pi-coding-agent";
import type { EditorTheme, KeybindingsManager, TUI } from "@earendil-works/pi-tui";

class FramedEditor extends CustomEditor {
  constructor(
    tui: TUI,
    theme: EditorTheme,
    keybindings: KeybindingsManager,
    private readonly color: (text: string) => string,
  ) {
    super(tui, theme, keybindings, { paddingX: 3 });
  }

  private showPrompt = true;

  override setPaddingX(_padding: number): void {
    super.setPaddingX(3);
  }

  protected override renderTopBorder(width: number, hiddenLineCount: number): string {
    this.showPrompt = hiddenLineCount === 0;
    if (width < 6 || hiddenLineCount > 0 || this.isShowingAutocomplete()) {
      return super.renderTopBorder(width, hiddenLineCount);
    }
    return (
      this.borderColor(`┏${"━".repeat(width - 5)} `) + this.color("✦") + this.borderColor(" ┓")
    );
  }

  protected override renderBottomBorder(width: number, hiddenLineCount: number): string {
    if (width < 6 || hiddenLineCount > 0 || this.isShowingAutocomplete()) {
      return super.renderBottomBorder(width, hiddenLineCount);
    }
    return this.borderColor(`┗${"━".repeat(width - 2)}┛`);
  }

  override render(width: number): string[] {
    const lines = super.render(width);
    if (width >= 7 && this.showPrompt) {
      const firstLine = lines[1]!;
      lines[1] = firstLine.slice(0, 1) + this.color("❯") + firstLine.slice(2);
    }
    if (width < 6 || this.isShowingAutocomplete()) return lines;

    for (let index = 1; index < lines.length - 1; index++) {
      const line = lines[index]!;
      lines[index] = this.borderColor("┃") + line.slice(1, -1) + this.borderColor("┃");
    }
    return lines;
  }
}

export default function (pi: ExtensionAPI) {
  pi.on("session_start", (_event, ctx) => {
    if (ctx.mode !== "tui") return;

    ctx.ui.setEditorComponent(
      (tui, theme, keybindings) =>
        new FramedEditor(tui, theme, keybindings, (text) => ctx.ui.theme.fg("borderAccent", text)),
    );
  });
}
