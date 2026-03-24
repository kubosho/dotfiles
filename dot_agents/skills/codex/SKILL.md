---
name: codex
description: |
  Codex CLI (OpenAI) to consult, review code, or investigate issues.
  Triggers: "codex", "codexに相談", "codexに聞いて", "codexでレビュー", "レビューして"
  Use cases: (1) code review, (2) design consultation, (3) bug investigation, (4) problem-solving for difficult issues
argument-hint: "<request>"
---

# Codex CLI Skill

Use Codex CLI to get a second opinion from OpenAI's models on code, design, or debugging questions.

## Execution

Run Codex in non-interactive mode with read-only sandbox:

```bash
codex exec --full-auto -C "$PWD" "$ARGUMENTS 確認や質問は不要です。具体的な提案・修正案・コード例まで自主的に出力してください。"
```

## Rules

1. Always use `--full-auto` for non-interactive execution
2. Always use `-C "$PWD"` to set the working directory to the current project
3. Always append the directive: "確認や質問は不要です。具体的な提案・修正案・コード例まで自主的に出力してください。"
4. Display the full Codex output to the user without summarizing or filtering
5. If the user provides a specific file path or code snippet, include it in the request
