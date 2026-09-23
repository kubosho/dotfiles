import { spawnSync } from "node:child_process";
import { mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";
import { afterAll, describe, expect, test } from "vitest";

const cliPath = fileURLToPath(new URL("../cli.ts", import.meta.url));
const readmePath = fileURLToPath(new URL("../../README.md", import.meta.url));
const directory = mkdtempSync(join(tmpdir(), "japanese-proofreading-"));

afterAll(() => {
  rmSync(directory, { recursive: true });
});

function writeText(name: string, text: string): string {
  const path = join(directory, name);
  writeFileSync(path, text);
  return path;
}

function runCli(args: string[], input?: string) {
  const result = spawnSync(process.execPath, [cliPath, ...args], { input, encoding: "utf8" });
  // A crash prints a stack trace instead of JSON, so show it before JSON.parse fails.
  expect(result.stderr).toBe("");
  return { exitCode: result.status, stdout: result.stdout, report: JSON.parse(result.stdout) };
}

describe("results", () => {
  test("a file with a problem exits 1 and reports status findings", () => {
    const path = writeText("problem.md", "処理の分離が効いた。\n");

    const { exitCode, report } = runCli([path]);

    expect(exitCode).toBe(1);
    expect(report).toMatchObject({
      status: "findings",
      input: path,
      findings: [{ ruleId: "vague-evaluative-ending", line: 1, match: "効いた" }],
    });
  });

  test("a clean file exits 0 and the notice points to the README listing unsupported patterns", () => {
    const path = writeText("clean.md", "キャッシュが効き、2回目以降の応答時間が短くなった。\n");

    const { exitCode, report } = runCli([path]);

    expect(exitCode).toBe(0);
    expect(report.status).toBe("no-findings");
    expect(report.findings).toEqual([]);
    expect(report.notice).toContain(readmePath);
  });

  test("- reads the text from stdin", () => {
    const { exitCode, report } = runCli(["-"], "設計書では不存在と同じ 404 に寄せている。");

    expect(exitCode).toBe(1);
    expect(report.findings).toMatchObject([{ ruleId: "nominalized-kango", match: "不存在" }]);
  });

  test("two runs on the same file print identical output", () => {
    const path = writeText(
      "repeat.md",
      "FIXME のヒントが要るなら devnavi-init を呼んでもらえれば置く。\n技術資産 ── すでにあるもの\n",
    );

    expect(runCli([path]).stdout).toBe(runCli([path]).stdout);
  });
});

describe("failures", () => {
  test("a missing file exits 2 with status failed instead of no findings", () => {
    const { exitCode, report } = runCli([join(directory, "missing.md")]);

    expect(exitCode).toBe(2);
    expect(report.status).toBe("failed");
    expect(report.error).toContain("missing.md");
  });

  test("no file argument exits 2 even when text is piped to stdin", () => {
    const { exitCode, report } = runCli([], "処理の分離が効いた。");

    expect(exitCode).toBe(2);
    expect(report.status).toBe("failed");
  });

  test("an unknown option exits 2", () => {
    const path = writeText("unknown-option.md", "本文。\n");

    const { exitCode, report } = runCli(["--fix", path]);

    expect(exitCode).toBe(2);
    expect(report.status).toBe("failed");
  });
});
