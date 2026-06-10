---
name: spec-implementation-check
description: Check the current implementation diff against a docs/specs spec. Use before committing a completed AC, or when asked to run spec-implementation-check.
argument-hint: "<docs/specs/spec-file.md>"
---

Check the current implementation diff against the given spec.

If no spec path is provided, ask for it.

1. Run the project's narrowest relevant test command.
2. Read the current diff.
3. Ask `negative-requirements-reviewer` to compare the diff with the spec's negative requirements.
4. Run `~/.agents/scripts/spec-scope-check.sh <spec-path>` to compare the current diff with the spec's scope limit.
5. Report Axis A, Axis B, Axis C, and the final judgement.

If no project-defined test command exists, mark Axis A skipped only for documentation-only work or with user approval.

Do not create GitHub Issues, `docs/tasks`, Agent Teams, dependency-order blockers, or front-matter consistency tools.

When the judgement is a return, classify the failure:

- 変換エラー: the spec is right and the code mistranscribed it. Fix locally and stay in implementation.
- 仕様欠陥: implementation revealed an ambiguity or error in the spec. Return to spec-authoring, fix the spec in its own commit, then re-implement.

Treat an Axis C overflow candidate as a 仕様欠陥 signal, not merely a volume problem: the design grew during implementation, which means the spec was not settled.

Report:

- Axis A: passed / failed / skipped
- Axis B: no candidates / candidates / skipped
- Axis C: no candidates / candidates / skipped
- Judgement: commit allowed / human judgement needed / return to implementation (変換エラー) / return to spec (仕様欠陥)
