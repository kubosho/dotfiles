import type { Usage } from "@earendil-works/pi-ai";
import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { truncateToWidth } from "@earendil-works/pi-tui";

import {
  barSegments,
  contextPercentage,
  formatRateLimitWindow,
  parseCodexRateLimits,
  type RateLimitWindow,
  usageColor,
} from "./format.ts";

const RGB = {
  green: [151, 201, 195],
  yellow: [229, 192, 123],
  red: [224, 108, 117],
  gray: [123, 143, 150],
  orange: [217, 119, 87],
  gold: [255, 215, 0],
} as const;

function color(name: keyof typeof RGB, text: string): string {
  const [red, green, blue] = RGB[name];
  return `\x1b[38;2;${red};${green};${blue}m${text}\x1b[0m`;
}

function usageBar(percentage: number | null, width: number): string {
  const { filled, empty } = barSegments(percentage ?? 0, width);
  const filledBar = filled > 0 ? color(usageColor(percentage ?? 0), "█".repeat(filled)) : "";
  return filledBar + color("gray", "░".repeat(empty));
}

function rateLimitDisplay(window: RateLimitWindow): string {
  const percentage = Math.round(window.usedPercentage);
  const colorName = usageColor(percentage);
  return (
    color(colorName, `${formatRateLimitWindow(window.windowMinutes)}:`) +
    ` ${usageBar(percentage, 8)} ` +
    color(colorName, `${percentage}%`)
  );
}

function addUsage(total: { cost: number }, usage: Usage): void {
  total.cost += usage.cost.total;
}

export default function (pi: ExtensionAPI) {
  let rateLimits: RateLimitWindow[] = [];
  let requestRender: (() => void) | undefined;

  pi.on("after_provider_response", (event) => {
    const nextRateLimits = parseCodexRateLimits(event.headers);
    if (nextRateLimits.length === 0) return;

    rateLimits = nextRateLimits;
    requestRender?.();
  });

  pi.on("session_start", (_event, ctx) => {
    if (ctx.mode !== "tui") return;

    ctx.ui.setFooter((tui, _theme, footerData) => {
      const unsubscribe = footerData.onBranchChange(() => tui.requestRender());
      requestRender = () => tui.requestRender();

      return {
        dispose() {
          unsubscribe();
          requestRender = undefined;
        },
        invalidate() {},
        render(width: number): string[] {
          const total = { cost: 0 };

          for (const entry of ctx.sessionManager.getEntries()) {
            if (entry.type === "usage") {
              addUsage(total, entry.usage);
            } else if (
              entry.type === "message" &&
              (entry.message.role === "assistant" || entry.message.role === "toolResult") &&
              entry.message.usage
            ) {
              addUsage(total, entry.message.usage);
            } else if (
              (entry.type === "compaction" || entry.type === "branch_summary") &&
              entry.usage
            ) {
              addUsage(total, entry.usage);
            }
          }

          const percentage = contextPercentage(ctx.getContextUsage()?.percent);
          const percentageText =
            percentage == null
              ? color("gray", "--%")
              : color(usageColor(percentage), `${percentage}%`);
          const separator = color("gray", " │ ");
          const model = ctx.model?.name ?? ctx.model?.id ?? "Unknown";
          const thinking = ctx.model?.reasoning ? (ctx.thinkingLevel ?? "off") : "off";

          const line1 =
            color("orange", `🤖 ${model}`) +
            " " +
            color("gray", `🧠 ${thinking}`) +
            separator +
            color("gray", "📊 ") +
            usageBar(percentage, 10) +
            ` ${percentageText}` +
            separator +
            color("gold", `💰 $${total.cost.toFixed(2)}`);

          const line2 =
            color("gray", "⏳ ") +
            (rateLimits.length > 0
              ? rateLimits.map(rateLimitDisplay).join(separator)
              : color("gray", "Codex: --"));

          return [truncateToWidth(line1, width), truncateToWidth(line2, width)];
        },
      };
    });
  });
}
