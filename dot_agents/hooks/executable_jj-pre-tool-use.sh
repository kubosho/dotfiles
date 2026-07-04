#!/bin/bash
# PreToolUse hook for Bash tool
# - jj git push: run git hooks (lefthook/husky), provide bookmark context
# - jj describe: provide diff context for single-context verification

set -euo pipefail

# exit 0 = skip hook silently (no block, no context injection)
# Conditions: jq/jj not installed, cwd inaccessible, not a jj repo, or command not matching jj patterns below
# exit 2 = block the tool call (used when git hooks fail)
command -v jq >/dev/null 2>&1 || exit 0
command -v jj >/dev/null 2>&1 || exit 0

INPUT=$(cat)
COMMAND=$(echo "$INPUT" | jq -r '.tool_input.command // ""')
CWD=$(echo "$INPUT" | jq -r '.cwd')

cd "$CWD" 2>/dev/null || exit 0
[[ -d ".jj" ]] || exit 0

# Skip the perl fork below for the common case of commands with no "jj" substring
# at all, since stripping quotes/heredocs can only remove characters, never add
# a "jj" that wasn't already there.
case "$COMMAND" in
  *jj*) ;;
  *) exit 0 ;;
esac

# Strip heredoc bodies and quoted string contents before matching, so data appended
# to unrelated files (e.g. a diary entry mentioning "jj describe") or a pattern
# passed to another command (e.g. `grep "jj git push"`) isn't mistaken for an
# actual jj invocation. Falls back to the raw command if perl is unavailable.
if command -v perl >/dev/null 2>&1; then
  MATCH_TARGET=$(printf '%s' "$COMMAND" | perl -0777 -pe '
    s/<<-?\s*(["\x27]?)(\w+)\1[^\n]*\n.*?\n\s*\2\b//gs;
    s/"(?:[^"\\]|\\.)*"//gs;
    s/'"'"'(?:[^'"'"'\\]|\\.)*'"'"'//gs;
  ')
else
  MATCH_TARGET="$COMMAND"
fi

case "$MATCH_TARGET" in
  *"jj git push"*)
    if [[ -f "lefthook.yml" ]]; then
      OUTPUT=$(lefthook run pre-commit 2>&1) || {
        printf 'lefthook pre-commit failed:\n%s' "$OUTPUT" >&2
        exit 2
      }
      OUTPUT=$(lefthook run pre-push 2>&1) || {
        printf 'lefthook pre-push failed:\n%s' "$OUTPUT" >&2
        exit 2
      }
    elif [[ -d ".husky" ]]; then
      for hook in pre-commit pre-push; do
        if [[ -x ".husky/$hook" ]]; then
          OUTPUT=$(bash ".husky/$hook" 2>&1) || {
            printf 'husky %s failed:\n%s' "$hook" "$OUTPUT" >&2
            exit 2
          }
        fi
      done
    fi

    LOG=$(jj log -r '::@ ~ root()' --limit 10 2>/dev/null || echo "")
    if [[ -n "$LOG" ]]; then
      jq -n --arg ctx "[jj-workflow] Verify bookmarks exist on push targets before proceeding. If no bookmark on target revision, create one: jj bookmark create <name> -r <rev>
jj log:
$LOG" \
        '{hookSpecificOutput: {hookEventName: "PreToolUse", additionalContext: $ctx}}'
    fi
    ;;

  *"jj describe"*)
    DIFF=$(jj diff --stat 2>/dev/null || echo "")
    if [[ -n "$DIFF" ]]; then
      jq -n --arg ctx "[jj-workflow] Verify changes are a single logical context. If multiple contexts exist (describable only with \"and\"), split first: jj split -m \"type: summary\" <filesets>
jj diff --stat:
$DIFF" \
        '{hookSpecificOutput: {hookEventName: "PreToolUse", additionalContext: $ctx}}'
    fi
    ;;
esac

exit 0
