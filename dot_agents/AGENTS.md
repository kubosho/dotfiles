# Personal Preferences

## Writing Style

- Do not use em dashes (—) or semicolons (;) in any output (chat responses, commit messages, PR bodies, code comments). Use commas, periods, or separate sentences instead.
- The vocabulary you built up while working is yours, not theirs; leave it behind unless you re-introduce it.
- When you write the summary at the end, drop the working shorthand. Write complete sentences. Spell out terms. Don't use arrow chains, hyphen-stacked compounds, or labels you made up earlier. When you mention files, commits, flags, or other identifiers, give each one its own plain-language clause. Open with the outcome: one sentence on what happened or what you found. Then the supporting detail. If you have to choose between short and clear, choose clear.
- 日本語の出力では、法律・行政・出版など他分野の硬い語（「正本」「未決」「憲法」「布告」など）を、比喩や格付けとして使わない。「唯一の参照元」「最優先のルール」「未確定」のように、指す内容をそのまま書く。
- 「効く」は、辞書に載っている次の五つの意味の場面に限って使う。効果や働きが現れる（薬が効く）、本来の機能を発揮する（鼻が効く）、それをすることが可能である（学割が効く）、口を利く、腕が立つ。それ以外の場面では、「適用される」「対象になる」「反映される」「届く」など、その場面に合った語を選ぶ。他に当てはまる語がどうしても見つからないときだけ「効く」を使う。

## Reasoning

After receiving the tool results, carefully reflect on their quality and determine the optimal next steps before proceeding. Use your thought process to plan and iterate based on this new information, and then take the best next action.

Extended thinking increases latency and should be reserved for cases where it significantly improves output quality, such as complex multi-step reasoning. If unsure, respond directly.

## Plan Files

- Use human-readable filenames for plan files in $XDG_DATA_HOME/obsidian/Plans/ directory
- Format: `YYYY-MM-DD-task-name.md` (e.g., 2026-01-14-add-user-auth.md)
- Use `$XDG_DATA_HOME/obsidian/Plans/_template.md` as the template when creating a new plan file

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
