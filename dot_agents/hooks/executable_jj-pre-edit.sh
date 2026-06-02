#!/bin/bash
# PreToolUse hook for file-modification tools (Edit/Write)
# 現コミットの動機を編集の直前に差し出し、これから書く変更が
# 同じ動機の延長線にあるかどうかをエージェント自身に問わせる

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
# 動機が名付けられていない作業コピーには境界の問いを発しない (これから付ける場面なので)
[[ -z "$DESC" ]] && exit 0

jq -n --arg ctx "[jj-workflow] 現在の作業コピーは「${DESC}」という動機で名付けられている。コミットは一つの動機を担う器であり、これから書く変更がその動機の延長線にあるなら自然と整う。別の動機がここで立ち上がっているなら、境界をいま引き直すのが後から履歴を読む者への誠実さになる。判断はあなたに委ねられている。継続なら手を進め、別の動機なら jj new で新しい器を用意してから手を動かす。" \
  '{hookSpecificOutput: {hookEventName: "PreToolUse", additionalContext: $ctx}}'

exit 0
