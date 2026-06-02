#!/bin/bash
# Axis C: compare actual diff volume with a spec's scope limit.

set -euo pipefail

SPEC_FILE="${1:-}"
DIFF_STAT_FILE="${2:-}"
TOLERANCE="${AI_WORKFLOW_SCOPE_TOLERANCE:-1.25}"

[[ -n "$SPEC_FILE" ]] || {
  printf 'Usage: spec-scope-check.sh <spec-file> [diff-stat-file]\n' >&2
  exit 1
}

[[ -f "$SPEC_FILE" ]] || {
  printf '[ai-workflow] Spec file not found: %s\n' "$SPEC_FILE" >&2
  exit 1
}

scope_section=$(awk '
  /^##[[:space:]]+スコープ上限[[:space:]]*$/ { in_scope = 1; next }
  /^##[[:space:]]+/ && in_scope { exit }
  in_scope { print }
' "$SPEC_FILE")

[[ -n "$scope_section" ]] || exit 0

file_match=$(printf '%s\n' "$scope_section" | grep -Eo '[0-9]+[[:space:]]*ファイル' | head -n 1 || true)
line_match=$(printf '%s\n' "$scope_section" | grep -Eo '[0-9]+[[:space:]]*行' | head -n 1 || true)
file_limit=$(printf '%s\n' "$file_match" | grep -Eo '[0-9]+' || true)
line_limit=$(printf '%s\n' "$line_match" | grep -Eo '[0-9]+' || true)

[[ -n "$file_limit" || -n "$line_limit" ]] || exit 0

if [[ -n "$DIFF_STAT_FILE" ]]; then
  [[ -f "$DIFF_STAT_FILE" ]] || {
    printf '[ai-workflow] Diff stat file not found: %s\n' "$DIFF_STAT_FILE" >&2
    exit 1
  }
  diff_stat=$(cat "$DIFF_STAT_FILE")
else
  command -v jj >/dev/null 2>&1 || exit 0
  diff_stat=$(jj diff --stat 2>/dev/null || true)
fi

[[ -n "$diff_stat" ]] || exit 0

summary_line=$(printf '%s\n' "$diff_stat" | grep -E '[0-9]+ files? changed|[0-9]+ insertions?|[0-9]+ deletions?' | tail -n 1 || true)

actual_insertions=0
actual_deletions=0
actual_files=$(printf '%s\n' "$diff_stat" | grep -c '|' || true)

if [[ "$summary_line" =~ ([0-9]+)[[:space:]]+files?[[:space:]]+changed ]]; then actual_files="${BASH_REMATCH[1]}"; fi
if [[ "$summary_line" =~ ([0-9]+)[[:space:]]+insertions? ]]; then
  actual_insertions="${BASH_REMATCH[1]}"
fi
if [[ "$summary_line" =~ ([0-9]+)[[:space:]]+deletions? ]]; then
  actual_deletions="${BASH_REMATCH[1]}"
fi

actual_lines=$((actual_insertions + actual_deletions))

ceil_with_tolerance() {
  awk -v limit="$1" -v tolerance="$TOLERANCE" 'BEGIN {
    value = limit * tolerance
    rounded = int(value)
    if (value > rounded) {
      rounded += 1
    }
    print rounded
  }'
}

expected_parts=()
actual_parts=()
reason_parts=()

if [[ -n "$file_limit" ]]; then
  file_threshold=$(ceil_with_tolerance "$file_limit")
  expected_parts+=("ファイル数 ${file_limit} 前後（判定しきい値 ${file_threshold}）")
  actual_parts+=("ファイル数 ${actual_files}")
  if (( actual_files > file_threshold )); then
    reason_parts+=("ファイル数が ${actual_files} で、しきい値 ${file_threshold} を超過")
  fi
fi

if [[ -n "$line_limit" ]]; then
  line_threshold=$(ceil_with_tolerance "$line_limit")
  expected_parts+=("総行数 ${line_limit} 行以内（判定しきい値 ${line_threshold}）")
  actual_parts+=("総行数 ${actual_lines} 行")
  if (( actual_lines > line_threshold )); then
    reason_parts+=("総行数が ${actual_lines} 行で、しきい値 ${line_threshold} を超過")
  fi
fi

((${#reason_parts[@]} > 0)) || exit 0

join_by() {
  local delimiter="$1"
  shift
  local first=1
  for item in "$@"; do
    if (( first )); then
      printf '%s' "$item"
      first=0
    else
      printf '%s%s' "$delimiter" "$item"
    fi
  done
}

cat <<EOF
## 軸C：スコープ超過候補

- 仕様：${SPEC_FILE} のスコープ上限
- 期待：$(join_by '、' "${expected_parts[@]}")
- 実測：$(join_by '、' "${actual_parts[@]}")
- 判定：超過候補
- 根拠：$(join_by '、' "${reason_parts[@]}")
- 残課題：変更量が仕様上妥当かを人間が判定する
EOF
