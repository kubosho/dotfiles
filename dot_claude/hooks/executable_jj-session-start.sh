#!/bin/bash
# SessionStart hook
# Resolve undescribed changes upfront so the Stop hook doesn't cry wolf all session

set -euo pipefail

command -v jq >/dev/null 2>&1 || exit 0
command -v jj >/dev/null 2>&1 || exit 0

INPUT=$(cat)
CWD=$(echo "$INPUT" | jq -r '.cwd')

cd "$CWD" 2>/dev/null || exit 0
[[ -d ".jj" ]] || exit 0

HAS_DIFF=$(jj diff 2>/dev/null || echo "")
[[ -z "$HAS_DIFF" ]] && exit 0

DESC=$(jj log --no-graph -r @ -T 'description' 2>/dev/null || echo "")
[[ -n "$DESC" ]] && exit 0

STAT=$(jj diff --stat 2>/dev/null || echo "")

jq -n --arg ctx "[jj-workflow] Working copy has undescribed changes. Ask the user how to handle them before starting any work. Use AskUserQuestion with these options:
1. jj describe -m \"...\" — describe the changes
2. jj abandon — discard the changes
3. jj new <base> — leave them as-is and start a new working copy elsewhere

Changed files:
$STAT" \
  '{hookSpecificOutput: {hookEventName: "SessionStart", additionalContext: $ctx}}'

exit 0
