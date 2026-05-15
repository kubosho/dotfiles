#!/bin/bash
# WorktreeCreate hook
# Delegates to `jj workspace add` so Claude Code's worktree feature operates on
# jj workspaces. Only the final stdout line is the absolute path. Everything
# else must go to stderr to avoid polluting the path Claude Code reads back.

set -euo pipefail

command -v jq >/dev/null 2>&1 || exit 0
command -v jj >/dev/null 2>&1 || exit 0

INPUT=$(cat)
NAME=$(echo "$INPUT" | jq -r '.name // empty')
CWD=$(echo "$INPUT" | jq -r '.cwd // empty')

REPO_DIR="${CWD:-$PWD}"

REPO_ROOT=$(cd "$REPO_DIR" && jj root 2>/dev/null) || {
  echo "Not a jj repository: $REPO_DIR" >&2
  exit 1
}

if [[ -z "$NAME" ]]; then
  NAME="wt-$(date +%s | tail -c 6)"
fi

WORKSPACE_DIR="${REPO_ROOT}/.claude/worktrees/${NAME}"

mkdir -p "$(dirname "$WORKSPACE_DIR")"
echo "Creating jj workspace '${NAME}' at ${WORKSPACE_DIR}" >&2
(cd "$REPO_ROOT" && jj workspace add "$WORKSPACE_DIR" --name "$NAME") >&2

echo "$WORKSPACE_DIR"
