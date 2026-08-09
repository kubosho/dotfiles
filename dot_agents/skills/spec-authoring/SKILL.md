---
name: spec-authoring
description: Draft or refine a docs/specs spec from a user goal or rough plan. Use when creating a new spec, turning a plan into acceptance criteria, or updating an existing spec.
argument-hint: "<goal, rough plan, or docs/specs/spec-file.md>"
---

Draft or refine a spec under `docs/specs/`, relative to the project root (`jj root`).

Follow `~/.docs/specs/_template.md` when it exists. If it does not exist, use the same structure.

The user owns and approves the purpose, acceptance criteria, negative requirements, scope limit, and final spec.

Draft, refine, and point out ambiguity. Ask for missing decisions one at a time, but inspect the codebase instead when it can answer the question.

Before declaring the spec settled, verify:

- The purpose section states the current problem, the solution applied, and what to build, in that order, in a form that can be skimmed. Design decisions do not accumulate in the purpose section.
- Every design decision carries its reason, written as a nested list item directly under the decision, in negative requirements or technical constraints. Decisions without reasons have caused fabricated reasons in later rewrites.
- Each AC bullet is written as an observable outcome (input or operation, then expected result), with no design decisions left to the implementer.
- A failing test can be written directly from each AC bullet, before any implementation code.
- No unresolved items (TBD, 要検討, 〜かもしれない) remain in the spec body.
- The scope limit has concrete numbers.

While any check fails, the spec is not settled and implementation does not start.

Do not create GitHub Issues or `docs/tasks`. Keep tasks inside the spec file.
