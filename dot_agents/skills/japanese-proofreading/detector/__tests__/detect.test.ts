import { describe, expect, test } from "vitest";

import { detect } from "../detect.ts";

describe("findings", () => {
  test("a sentence closed by 効いた on line 2 is reported at its line and column", () => {
    const findings = detect("# 振り返り\n今回は処理の分離が効いた。");

    expect(findings).toEqual([
      {
        ruleId: "vague-evaluative-ending",
        pattern: "異なる結果を汎用的な評価語で閉じる",
        line: 2,
        column: 10,
        match: "効いた",
        sentence: "今回は処理の分離が効いた。",
        reason: expect.any(String),
        hint: expect.any(String),
        requiredInformation: ["変化した対象", "観察された結果"],
      },
    ]);
  });

  test("findings from different rules are ordered by position in the text", () => {
    const findings = detect(
      "設計書では不存在と同じ 404 に寄せている。\n4エージェントに同じ骨格で書き直させます。",
    );

    expect(findings.map(({ ruleId, line }) => [ruleId, line])).toEqual([
      ["nominalized-kango", 1],
      ["calqued-metaphor", 2],
    ]);
  });

  test("無言で降りる lists the actual behavior as required information", () => {
    const [finding] = detect("抽出できない形では、誤った差分を出すより無言で降りるほうが安全。");

    expect(finding.match).toBe("無言で降り");
    expect(finding.requiredInformation).toEqual([
      "実際の動作（戻り値、終了コード、出力の有無と出力先など）",
    ]);
  });
});

describe("text excluded from checks", () => {
  test.each([
    ["a bad example quoted in 「」", "「処理の分離が効いた」のような書き方を避ける。"],
    ["a bad example in 『』 inside 「」", "「『過積載』と書いた」例を載せる。"],
    ["inline code", "`無言で降りる` のような表現を避ける。"],
    ["inline code in double backticks", "``無言で降りる`` のような表現を避ける。"],
    ["a fenced code block", "```\n骨格を作る\n```"],
    ["a ~~~ fenced code block", "~~~\n骨格を作る\n~~~"],
    ["a ``` line inside a ```` fence", "````\n```\n骨格を作る\n````"],
    ["a ``` line inside a ~~~ fence", "~~~\n```\n骨格を作る\n~~~"],
    ["a blockquote line", "> 処理の分離が効いた。"],
    ["an HTML comment", "<!-- 不存在 -->"],
    ["frontmatter", "---\ntitle: 不存在\n---\n本文。"],
  ])("%s is not reported", (_, text) => {
    expect(detect(text)).toEqual([]);
  });

  test("an unclosed 「 does not hide the rest of the line", () => {
    expect(detect("「処理の分離が効いた。").map(({ match }) => match)).toEqual(["効いた"]);
  });

  test("text after a fenced code block is checked again", () => {
    expect(detect("```\ncode\n```\n不存在").map(({ match }) => match)).toEqual(["不存在"]);
  });

  test("text between horizontal rules in the body is checked", () => {
    expect(detect("本文。\n\n---\n\n不存在\n\n---").map(({ match }) => match)).toEqual(["不存在"]);
  });
});

describe("edge cases", () => {
  test("empty text has no findings", () => {
    expect(detect("")).toEqual([]);
  });

  test("column counts a character outside the BMP as one", () => {
    const [finding] = detect("𠮷野家は不存在。");

    expect(finding.column).toBe(5);
  });
});
