---
name: spec-authoring
description: Draft or refine a docs/specs spec from a user goal or rough plan. Use when creating a new spec, turning a plan into acceptance criteria, or updating an existing spec.
argument-hint: "<goal, rough plan, or docs/specs/spec-file.md>"
---

Draft or refine a spec under `docs/specs/`, relative to the project root (`jj root`).

Follow `~/.docs/specs/_template.md` when it exists. If it does not exist, use the same structure.

The user owns and approves the purpose, acceptance criteria, negative requirements, scope limit, and final spec.

Draft, refine, and point out ambiguity. Ask for missing decisions one at a time, but inspect the codebase instead when it can answer the question.

Before declaring the spec settled, verify every AC:

- 入力 → 結果 is written as an observable outcome, with no design decisions left to the implementer.
- A failing test can be written directly from the 入力 → 結果 line, before any implementation code.
- No unresolved items (TBD, 要検討, 〜かもしれない) remain in the spec body.
- The scope limit has concrete numbers.

While any AC fails these checks, the spec is not settled and implementation does not start.

Do not create GitHub Issues or `docs/tasks`. Keep tasks inside the spec file.
