# Jujutsu (jj) Workflow

Applies when `.jj` exists in the project root. The vcs-overview SessionStart hook auto-reports per-subdir VCS state at session start, so the activation condition does not need a manual probe.

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

Some invariants are enforced by hooks (`~/.agents/hooks/*.sh`). Those marked **[hook]** are automated.

### Pre-existing undescribed changes are resolved at session start **[hook: SessionStart]**

Use AskUserQuestion to ask the user how to handle them before starting any work. Present only the options listed in the hook message (describe, abandon, or start new working copy).

### Working copy belongs to the current task **[hook: PreToolUse]**

The git-prefer-jj hook blocks `git checkout -b` / `git switch -c` / `git branch <name>` / `git commit` in jj repos and points at the jj equivalent.

Use the jj equivalents:

- New task → `jj new <base>` where `<base>` is the branch the task depends on (default: `main`)
- Commit → `jj describe -m "type: summary"` (or `jj new -m "..."` to stack a new commit)

### Bookmarks stay attached after rewrites **[hook: PostToolUse]**

After `jj squash`, the hook provides `jj log`. Act on it:

- Attached → proceed
- Detached (`<name>@origin` only, no local counterpart) → `jj bookmark set <name> -r <rev>`

### Each commit contains one logical context **[hook: PreToolUse]**

Before `jj describe`, the hook provides `jj diff --stat`. Use the output to judge:

- Single context → proceed
- Multiple contexts (describable only with "and") → `jj split -m "type: summary" <filesets>` to separate by context, then describe remaining commit separately
- Changes address multiple review comments → each comment is a separate context, split per comment
- Executing a plan with Phase/Step/numbered headings → commit per top-level heading
