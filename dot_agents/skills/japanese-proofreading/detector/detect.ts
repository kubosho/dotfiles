import { rules } from "./rules.ts";

export type Finding = {
  ruleId: string;
  pattern: string;
  line: number;
  column: number;
  match: string;
  sentence: string;
  reason: string;
  hint: string;
  requiredInformation: string[];
};

export type Sentence = {
  start: number;
  /** The sentence with non-prose parts blanked out, keeping every offset. */
  prose: string;
  original: string;
};

export function detect(text: string): Finding[] {
  const sentences = splitSentences(text);

  return rules
    .flatMap((rule) =>
      rule.check(sentences).map(({ sentence, index, length, ...message }) => {
        const offset = sentence.start + index;
        return {
          ruleId: rule.id,
          pattern: rule.pattern,
          ...position(text, offset),
          match: text.slice(offset, offset + length),
          sentence: sentence.original.trim(),
          reason: message.reason,
          hint: message.hint,
          requiredInformation: message.requiredInformation,
        };
      }),
    )
    .sort((a, b) => a.line - b.line || a.column - b.column);
}

function splitSentences(text: string): Sentence[] {
  const prose = maskNonProse(text);

  return Array.from(prose.matchAll(/[^\n。！？!?]+[。！？!?]*/g), (match) => ({
    start: match.index,
    prose: match[0],
    original: text.slice(match.index, match.index + match[0].length),
  }));
}

function blank(text: string): string {
  return text.replace(/[^\n]/g, " ");
}

function maskNonProse(text: string): string {
  let fence: string | undefined;

  return text
    .replace(/^---\n[\s\S]*?\n---(?=\n|$)/, blank)
    .replace(/<!--[\s\S]*?-->/g, blank)
    .split("\n")
    .map((line) => {
      const marker = line.match(/^\s*(`{3,}|~{3,})/)?.[1];
      if (fence !== undefined) {
        const closing = line.match(/^\s*(`{3,}|~{3,})\s*$/)?.[1];
        if (closing?.[0] === fence[0] && closing.length >= fence.length) fence = undefined;
        return blank(line);
      }
      if (marker !== undefined) {
        fence = marker;
        return blank(line);
      }
      if (/^\s*>/.test(line)) return blank(line);
      return maskQuotes(line.replace(/(`+)(?!`)[^\n]*?[^`\n]\1(?!`)/g, blank));
    })
    .join("\n");
}

function maskQuotes(line: string): string {
  let masked = line;
  let depth = 0;
  let start = 0;

  for (let i = 0; i < line.length; i++) {
    if (line[i] === "「" || line[i] === "『") {
      if (depth === 0) start = i;
      depth++;
    } else if ((line[i] === "」" || line[i] === "』") && depth > 0) {
      depth--;
      if (depth === 0) {
        masked = masked.slice(0, start) + blank(line.slice(start, i + 1)) + masked.slice(i + 1);
      }
    }
  }

  return masked;
}

function position(text: string, offset: number): { line: number; column: number } {
  const before = text.slice(0, offset);
  const lineStart = before.lastIndexOf("\n") + 1;
  return {
    line: before.split("\n").length,
    column: Array.from(before.slice(lineStart)).length + 1,
  };
}
