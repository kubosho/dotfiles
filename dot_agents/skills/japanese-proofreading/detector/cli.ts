import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { parseArgs } from "node:util";

import { detect } from "./detect.ts";

const readmePath = fileURLToPath(new URL("../README.md", import.meta.url));

function print(report: object): void {
  process.stdout.write(`${JSON.stringify(report, null, 2)}\n`);
}

try {
  const { positionals } = parseArgs({ allowPositionals: true });

  if (positionals.length !== 1) {
    throw new Error("検査するファイルを1つ指定する。標準入力から読む場合は - を指定する。");
  }

  const [input] = positionals;
  const findings = detect(readFileSync(input === "-" ? 0 : input, "utf8"));

  print({
    status: findings.length > 0 ? "findings" : "no-findings",
    input,
    findings,
    notice: `未対応の形式があるため、指摘がなくても文章全体に問題がないとは限らない。対応範囲は ${readmePath} を参照。`,
  });
  process.exitCode = findings.length > 0 ? 1 : 0;
} catch (error) {
  print({ status: "failed", error: (error as Error).message });
  process.exitCode = 2;
}
