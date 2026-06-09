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

jq -n --arg ctx "[jj-workflow] 現在の作業コピーは「${DESC}」という動機で名付けられている。コミットには一つの動機のみ担うべきなので、これから書く変更が別の動機になるならコミットを分割するのが、あとから履歴を読む者が迷わない。同一の動機なら引き続き手を動かし、別の動機なら jj new で新しいコミットを用意してから手を動かす。いま判断するべき。" \
  '{hookSpecificOutput: {hookEventName: "PreToolUse", additionalContext: $ctx}}'

exit 0
