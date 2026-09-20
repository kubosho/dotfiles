export type UsageColor = "green" | "yellow" | "red";

export interface RateLimitWindow {
  usedPercentage: number;
  windowMinutes: number;
}

export function usageColor(percentage: number): UsageColor {
  if (percentage < 50) return "green";
  if (percentage < 80) return "yellow";
  return "red";
}

export function barSegments(percentage: number, width: number): { filled: number; empty: number } {
  const filled = Math.max(0, Math.min(width, Math.floor((percentage * width) / 100)));
  return { filled, empty: width - filled };
}

export function contextPercentage(percent: number | null | undefined): number | null {
  return percent == null ? null : Math.round(percent);
}

export function parseCodexRateLimits(
  headers: Record<string, string | undefined>,
): RateLimitWindow[] {
  return ["primary", "secondary"]
    .map((name) => ({
      usedPercentage: Number(headers[`x-codex-${name}-used-percent`]),
      windowMinutes: Number(headers[`x-codex-${name}-window-minutes`]),
    }))
    .filter(
      ({ usedPercentage, windowMinutes }) =>
        Number.isFinite(usedPercentage) && Number.isFinite(windowMinutes) && windowMinutes > 0,
    )
    .sort((left, right) => left.windowMinutes - right.windowMinutes);
}

export function formatRateLimitWindow(minutes: number): string {
  if (minutes % 1_440 === 0) return `${minutes / 1_440}d`;
  if (minutes % 60 === 0) return `${minutes / 60}h`;
  return `${minutes}m`;
}
