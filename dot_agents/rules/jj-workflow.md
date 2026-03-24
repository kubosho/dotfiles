# Jujutsu (jj) Workflow

Applies when `.jj` exists in the project root.

## Outcome

Every commit carries a single reason and motivation for the change, making the history alone sufficient to trace intent.

## Constraints

- Claude Code has no TTY. Commands that open an interactive UI (editor launch, `--interactive` flag) always fail. Use non-interactive alternatives instead: `-m "message"` for commits, fileset arguments for splits, etc.
- Commit message format:
  ```
  type: summary
                          ← blank line
  body                    ← optional: omit if summary alone conveys the reason
                          ← blank line
  trailers                ← optional: e.g. Co-Authored-By
  ```
  - Types: feat, fix, refactor, docs, test, chore, perf, ci
  - Body: why the change was made, in the minimum words needed. No line breaks.

## Invariants

These conditions must hold true at each stage. When violated, the fix restores them.

Some invariants are enforced by hooks (`~/.claude/hooks/jj-*.sh`). Those marked **[hook]** are automated and do not require manual checks.

### Pre-existing undescribed changes are resolved at session start **[hook: SessionStart]**

A hook detects undescribed changes in the working copy when a session begins. Use AskUserQuestion to ask the user how to handle them (describe, abandon, or ignore) before starting any work. This prevents the Stop hook from repeatedly firing about changes unrelated to the current session.

### Working copy belongs to the current task

**Check**: `jj status` before file modifications.

- Matches current task → proceed
- Empty or different task → `jj new <base>` where `<base>` is the branch the task depends on (default: `main`)

### Bookmarks stay attached after rewrites **[hook: PostToolUse]**

A hook runs `jj log` after `jj squash` and provides context. Act on it:

- Attached → proceed
- Detached (`<name>@origin` only, no local counterpart) → `jj bookmark set <name> -r <rev>`

### Each commit contains one logical context **[hook: PreToolUse]**

A hook provides `jj diff --stat` before `jj describe`. Use the output to judge:

- Single context → proceed
- Multiple contexts (describable only with "and") → `jj split -m "type: summary" <filesets>` to separate by context, then describe remaining commit separately
- Changes address multiple review comments → each comment is a separate context, split per comment
