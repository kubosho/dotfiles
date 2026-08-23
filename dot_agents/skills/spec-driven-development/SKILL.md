---
name: spec-driven-development
description: How to run spec-driven development. Covers where the spec lives, the flow from spec-authoring through spec-implement to spec-implementation-check, and how AC progress and commits are handled. Use when starting spec-driven work, or when checking how to proceed.
---

Follow this workflow for spec-driven development.

## Managing the spec

- `docs/specs/` is the default location for a spec. Ask the user where it lives before writing, and place it in a GitHub Issue when the user chooses that.
- `docs/specs/` is relative to the project root (`jj root`).

## Implementation and verification

- `spec-authoring` writes and settles the spec, `spec-implement` implements one **acceptance criterion (AC)**, and `spec-implementation-check` verifies the implementation.
- Each AC is one task, one verification unit, and one jj commit context.
- Negative requirements are prohibitions. Do not implement anything listed there.
- Always run each AC's test, or its existence check when the AC has no runnable behavior, before marking work complete. If it does not succeed, the task is not complete.

## Progress and commits

- Keep AC progress in the spec's task list. Write an in-progress task as `- [ ] AC-N：（進行中）タスク名` and a finished one as `- [x] AC-N：タスク名`.
- Update the spec progress marker in the same commit as the implementation for that AC.
- Once every AC is implemented, set the spec status to `done`.
