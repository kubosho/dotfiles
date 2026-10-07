#!/bin/bash
# PostToolUse hook for Bash tool
# - jj squash: check bookmark attachment after rewrite

set -euo pipefail

# exit 0 = skip hook silently (no block, no context injection)
# Conditions: jq/jj not installed, cwd inaccessible, not a jj repo, or command is not jj squash
command -v jq >/dev/null 2>&1 || exit 0
command -v jj >/dev/null 2>&1 || exit 0

INPUT=$(cat)
COMMAND=$(echo "$INPUT" | jq -r '.tool_input.command // ""')
CWD=$(echo "$INPUT" | jq -r '.cwd')

cd "$CWD" 2>/dev/null || exit 0
[[ -d ".jj" ]] || exit 0

case "$COMMAND" in
  *"jj squash"*)
    LOG=$(jj log -r '::@ ~ root()' --limit 10 2>/dev/null || echo "")
    if [[ -n "$LOG" ]]; then
      jq -n --arg ctx "[jj-workflow] Bookmark attachment check after squash. If bookmarks are detached (showing only name@origin without local counterpart), fix with: jj bookmark set <name> -r <rev>
jj log:
$LOG" \
        '{hookSpecificOutput: {hookEventName: "PostToolUse", additionalContext: $ctx}}'
    fi
    ;;
esac

exit 0
