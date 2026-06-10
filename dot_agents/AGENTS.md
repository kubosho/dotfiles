# Personal Preferences

## Writing Style

- Do not use em dashes (—) or semicolons (;) in any output (chat responses, commit messages, PR bodies, code comments). Use commas, periods, or separate sentences instead.
- The vocabulary you built up while working is yours, not theirs; leave it behind unless you re-introduce it.
- When you write the summary at the end, drop the working shorthand. Write complete sentences. Spell out terms. Don't use arrow chains, hyphen-stacked compounds, or labels you made up earlier. When you mention files, commits, flags, or other identifiers, give each one its own plain-language clause. Open with the outcome: one sentence on what happened or what you found. Then the supporting detail. If you have to choose between short and clear, choose clear.

## Pull Requests

- Use the `pr-description` skill when writing PR body text

## Plan Files

- Use human-readable filenames for plan files in $XDG_DATA_HOME/obsidian/Plans/ directory
- Format: `YYYY-MM-DD-task-name.md` (e.g., 2026-01-14-add-user-auth.md)

## AI Agent Development Workflow

Use this workflow for spec-driven development.

- `docs/specs/` is the source of truth for specifications. Do not move tasks into GitHub Issues or `docs/tasks`.
- Each phase has a skill: `spec-authoring` writes and settles the spec, `spec-implement` implements one AC, `spec-implementation-check` verifies before commit.
- `docs/specs/` is relative to the project root (`jj root`). Written specs live there.
- Each acceptance criterion is one task, one verification unit, and one jj commit context.
- Keep AC progress in the spec file's task list. Use only GFM task boxes: `[ ]`, `[x]`, and `[ ] （進行中）`.
- Tests must always be run before marking work complete. If the required tests are not green, the task is not complete.
- Negative requirements are prohibitions. Do not implement anything listed there.
- Common conventions live in this file. AC-specific technical constraints live in the spec file.
- Update the spec progress marker in the same commit as the implementation for that AC.
