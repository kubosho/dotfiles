---
name: task-register
description: |
  Create a draft task file in Obsidian's 91_Tasks from a description. Use this whenever the user wants something recorded as a task instead of done now, including `/task-register <description>`, 「これタスクにしておいて」「後で見直すからタスク作って」「タスクに登録して」, or any request to park work as a task file, even when the word "task" is not used.
argument-hint: "<description of the task>"
---

Receive a task description and create a task file directly under `$XDG_DATA_HOME/obsidian/personal/91_Tasks/`.

## Steps

1. Use argument description as input. If missing, ask for task and stop.
2. Read `$XDG_DATA_HOME/obsidian/personal/00_Templates/タスク登録.md` to check structure and guidelines.
3. Assemble file name and section contents from description and context.
4. Run "Checklist" before saving.
5. Save to `$XDG_DATA_HOME/obsidian/personal/91_Tasks/<task name>.md`. If the file exists, inspect its current progress and ask for confirmation before overwriting.
6. Return file path, `status`, and count of criteria items. Mention any trimmed items or empty sections.

## File metadata

### File name

Name serves as task summary. Use a short name showing what to do, not the raw description.

Avoid characters problematic for paths/links like `/`, `:`, `\`.

### Frontmatter

Include only `status` and `review_date`.

- Default: `status: icebox`, `review_date` empty.
- If context implies start timing: `status: pending`, `review_date` as `YYYY-MM-DD`.

## Filling sections

- **概要**: Background/motivation from description and context.
- **達成すること**: Expected state or deliverables upon completion.
- **完了条件**: Follow quality standards below.
- **関連リソース**、**メモ**: Optional. Leave heading only if no content.

Do not guess. Leave heading empty if no input data.

## Criteria quality standards

Write criteria for your future self (or a colleague) starting cold, so no extra decisions are needed before taking action.

- **One action per item**: Items with multiple steps ("do X, Y, and Z") cause hesitation, which stalls momentum.
- **Max 25 mins per item (1 Pomodoro)**: Completing an item in one sitting keeps progress visible.
- **Actionable first item**: Start with something immediate (e.g., "read code" or "open file"). If it requires setup, it won't get started.
- **Max 5 items**: More than 5 is a project, not a task. Split it, or keep only what matters now and move the rest to Notes.
- **Use prose for single actions**: A one-item checklist adds overhead with no structural benefit. Use a simple paragraph instead.

## Checklist

Run this on your criteria before saving. If any check fails, fix it and restart the checklist.

- [ ] Does each item contain only one instruction? (Split if it uses "and").
- [ ] Is every item under 25 minutes?
- [ ] Can you start the first item immediately without prior setup?
- [ ] Are there 5 or fewer items?
