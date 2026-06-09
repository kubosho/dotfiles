#!/bin/bash
# PreToolUse hook for file-modification tools (Edit/Write)

set -euo pipefail

# exit 0 = skip hook silently
# Conditions: jq/jj not installed, cwd inaccessible, not a jj repo, or working copy has no description yet
command -v jq >/dev/null 2>&1 || exit 0
command -v jj >/dev/null 2>&1 || exit 0

INPUT=$(cat)
CWD=$(echo "$INPUT" | jq -r '.cwd')

cd "$CWD" 2>/dev/null || exit 0
[[ -d ".jj" ]] || exit 0

DESC=$(jj log --no-graph -r @ -T 'description.first_line()' 2>/dev/null || echo "")
[[ -z "$DESC" ]] && exit 0

jq -n --arg ctx "[jj-workflow] The current working copy is described as \"${DESC}\". A commit should carry one motivation. If the change you are about to write belongs to a different motivation, split now. Same motivation: proceed. Different motivation: run jj new first." \
  '{hookSpecificOutput: {hookEventName: "PreToolUse", additionalContext: $ctx}}'

exit 0
