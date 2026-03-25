#!/bin/bash
# Stop hook
# Blocks stop if working copy has undescribed changes

set -euo pipefail

# exit 0 = skip hook silently (allow stop)
# Conditions: jq/jj not installed, cwd inaccessible, not a jj repo, working copy has no changes, or working copy already has a description
# Note: working copy may have both a description and new changes if work continues after jj describe
command -v jq >/dev/null 2>&1 || exit 0
command -v jj >/dev/null 2>&1 || exit 0

INPUT=$(cat)
CWD=$(echo "$INPUT" | jq -r '.cwd')

cd "$CWD" 2>/dev/null || exit 0
[[ -d ".jj" ]] || exit 0

# jj diff (without --stat) outputs nothing when there are no changes
# jj diff --stat always outputs a summary line even with no changes
HAS_DIFF=$(jj diff 2>/dev/null || echo "")
[[ -z "$HAS_DIFF" ]] && exit 0

DESC=$(jj log --no-graph -r @ -T 'description' 2>/dev/null || echo "")

if [[ -z "$DESC" ]]; then
  STAT=$(jj diff --stat 2>/dev/null || echo "")
  jq -n --arg reason "[jj-workflow] Working copy has undescribed changes. Describe with: jj describe -m \"type: summary\"
Changed files:
$STAT" \
    '{decision: "block", reason: $reason}'
fi

exit 0
