import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import { detect, type Finding } from "./detect.ts";
import { rules } from "./rules.ts";

export type ExpectedFinding = Pick<Finding, "ruleId" | "match">;

export type Case = {
  id: string;
  text: string;
  expected: ExpectedFinding[];
};

export type CaseResult = {
  id: string;
  missed: ExpectedFinding[];
  unexpected: ExpectedFinding[];
};

export function loadCases(): Case[] {
  return JSON.parse(readFileSync(fileURLToPath(new URL("./cases.json", import.meta.url)), "utf8"));
}

export function evaluate(cases: Case[]): CaseResult[] {
  return cases.map((testCase) => {
    const actual = detect(testCase.text).map(({ ruleId, match }) => ({ ruleId, match }));
    return {
      id: testCase.id,
      missed: subtract(testCase.expected, actual),
      unexpected: subtract(actual, testCase.expected),
    };
  });
}

function subtract(from: ExpectedFinding[], remove: ExpectedFinding[]): ExpectedFinding[] {
  const remaining = remove.map(key);
  return from.filter((finding) => {
    const index = remaining.indexOf(key(finding));
    if (index === -1) return true;
    remaining.splice(index, 1);
    return false;
  });
}

function key({ ruleId, match }: ExpectedFinding): string {
  return JSON.stringify([ruleId, match]);
}

export function format(cases: Case[], results: CaseResult[]): string {
  const describe = (id: string, { ruleId, match }: ExpectedFinding) =>
    `  ${id}：${ruleId}「${match}」`;
  const list = (name: "missed" | "unexpected") => {
    const lines = results.flatMap((result) => result[name].map((f) => describe(result.id, f)));
    return lines.length > 0 ? lines : ["  なし"];
  };
  const count = (name: "missed" | "unexpected", ruleId: string) =>
    results.flatMap((result) => result[name]).filter((f) => f.ruleId === ruleId).length;

  return [
    `評価データ：${cases.length}件`,
    "",
    "規則ごとの件数",
    ...rules.map(({ id }) => {
      const expected = cases.flatMap((c) => c.expected).filter((f) => f.ruleId === id).length;
      return `  ${id}：期待 ${expected}、検出漏れ ${count("missed", id)}、誤検出 ${count("unexpected", id)}`;
    }),
    "",
    "検出漏れ",
    ...list("missed"),
    "",
    "誤検出",
    ...list("unexpected"),
  ].join("\n");
}

if (import.meta.main) {
  const cases = loadCases();
  const results = evaluate(cases);
  process.stdout.write(`${format(cases, results)}\n`);
  process.exitCode = results.some((r) => r.missed.length > 0 || r.unexpected.length > 0) ? 1 : 0;
}
