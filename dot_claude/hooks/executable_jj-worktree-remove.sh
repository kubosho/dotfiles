#!/bin/bash
# WorktreeRemove hook
# Forgets the jj workspace and removes its directory. `jj workspace forget`
# alone leaves the directory behind, so the rm step is required for cleanup.

set -euo pipefail

command -v jq >/dev/null 2>&1 || exit 0
command -v jj >/dev/null 2>&1 || exit 0

INPUT=$(cat)
WORKTREE_PATH=$(echo "$INPUT" | jq -r '.worktree_path // empty')

if [[ -z "$WORKTREE_PATH" ]]; then
  echo "No worktree_path provided" >&2
  exit 1
fi

WS_NAME=$(basename "$WORKTREE_PATH")
REPO_ROOT=$(cd "$WORKTREE_PATH" 2>/dev/null && jj root 2>/dev/null || echo "")

if [[ -n "$REPO_ROOT" ]]; then
  echo "Forgetting jj workspace '${WS_NAME}'" >&2
  (cd "$REPO_ROOT" && jj workspace forget "$WS_NAME" 2>/dev/null) >&2 || true
fi

if [[ -d "$WORKTREE_PATH" ]]; then
  echo "Removing worktree directory: ${WORKTREE_PATH}" >&2
  rm -rf "$WORKTREE_PATH"
fi
