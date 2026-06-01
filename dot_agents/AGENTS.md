# Personal Preferences

## Writing Style

- Do not use em dashes (—) or semicolons (;) in any output (chat responses, commit messages, PR bodies, code comments). Use commas, periods, or separate sentences instead.

## Pull Requests

- Use the `pr-description` skill when writing PR body text

## Plan Files

- Use human-readable filenames for plan files in $XDG_DATA_HOME/obsidian/Plans/ directory
- Format: `YYYY-MM-DD-task-name.md` (e.g., 2026-01-14-add-user-auth.md)

## AI Agent Development Workflow

Use this workflow for spec-driven development.

- `docs/specs/` is the source of truth for specifications. Do not move tasks into GitHub Issues or `docs/tasks`.
- `docs/specs/` is relative to the project root (`jj root`). Written specs live there.
- Each acceptance criterion is one task, one verification unit, and one jj commit context.
- Keep AC progress in the spec file's task list. Use only GFM task boxes: `[ ]`, `[x]`, and `[ ] （進行中）`.
- Tests must always be run before marking work complete. If the required tests are not green, the task is not complete.
- Negative requirements are prohibitions. Do not implement anything listed there.
- Common conventions live in this file. AC-specific technical constraints live in the spec file.
- Update the spec progress marker in the same commit as the implementation for that AC.
