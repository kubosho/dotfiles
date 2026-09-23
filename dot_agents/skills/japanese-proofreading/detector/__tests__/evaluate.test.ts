import { spawnSync } from "node:child_process";
import { fileURLToPath } from "node:url";
import { describe, expect, test } from "vitest";

import { evaluate, format, loadCases, type Case } from "../evaluate.ts";

describe("shipped evaluation data", () => {
  test("every case gets exactly the expected findings", () => {
    const mismatches = evaluate(loadCases()).filter(
      ({ missed, unexpected }) => missed.length > 0 || unexpected.length > 0,
    );

    expect(mismatches).toEqual([]);
  });

  test("running evaluate.ts exits 0 and lists no missed or unexpected findings", () => {
    const result = spawnSync(
      process.execPath,
      [fileURLToPath(new URL("../evaluate.ts", import.meta.url))],
      { encoding: "utf8" },
    );

    expect(result.stderr).toBe("");
    expect(result.status).toBe(0);
    expect(result.stdout).toContain("検出漏れ\n  なし\n\n誤検出\n  なし");
  });
});

describe("comparison", () => {
  test("an expected finding the detector does not return is listed as missed", () => {
    const [result] = evaluate([
      {
        id: "missed",
        text: "VRT の baseline 生成を CI に寄せる作業。",
        expected: [{ ruleId: "literal-verb-translation", match: "寄せる" }],
      },
    ]);

    expect(result).toEqual({
      id: "missed",
      missed: [{ ruleId: "literal-verb-translation", match: "寄せる" }],
      unexpected: [],
    });
  });

  test("a finding the case does not expect is listed as unexpected", () => {
    const [result] = evaluate([{ id: "unexpected", text: "不存在。", expected: [] }]);

    expect(result.unexpected).toEqual([{ ruleId: "nominalized-kango", match: "不存在" }]);
  });

  test("the same finding expected twice but returned once is missed once", () => {
    const expected = { ruleId: "nominalized-kango", match: "不存在" };

    const [result] = evaluate([{ id: "twice", text: "不存在。", expected: [expected, expected] }]);

    expect(result.missed).toEqual([expected]);
  });
});

describe("report", () => {
  const cases: Case[] = [
    {
      id: "missed-case",
      text: "処理の分離で変更範囲が狭まった。",
      expected: [{ ruleId: "vague-evaluative-ending", match: "効いた" }],
    },
    { id: "unexpected-case", text: "不存在。", expected: [] },
  ];

  test("missed and unexpected findings are listed under their headings with the case id", () => {
    const report = format(cases, evaluate(cases));

    expect(report).toContain("検出漏れ\n  missed-case：vague-evaluative-ending「効いた」");
    expect(report).toContain("誤検出\n  unexpected-case：nominalized-kango「不存在」");
  });

  test("each rule shows its expected, missed and unexpected counts", () => {
    const report = format(cases, evaluate(cases));

    expect(report).toContain("vague-evaluative-ending：期待 1、検出漏れ 1、誤検出 0");
    expect(report).toContain("nominalized-kango：期待 0、検出漏れ 0、誤検出 1");
  });
});
