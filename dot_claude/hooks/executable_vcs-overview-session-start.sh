#!/bin/bash
# SessionStart hook
# Report VCS state (jj/git) of immediate subdirectories under the workspace root.
# Helps Claude pick the right VCS tool per PJ without manual probing.

set -euo pipefail

command -v jq >/dev/null 2>&1 || exit 0

INPUT=$(cat)
CWD=$(echo "$INPUT" | jq -r '.cwd')

[[ -d "$CWD" ]] || exit 0

# Walk up to find the workspace root: a dir with mani.yaml or *.code-workspace.
# Fall back to the cwd itself.
ROOT=""
DIR="$CWD"
while [[ "$DIR" != "/" && "$DIR" != "." ]]; do
  if [[ -f "$DIR/mani.yaml" ]] || compgen -G "$DIR/*.code-workspace" >/dev/null 2>&1; then
    ROOT="$DIR"
    break
  fi
  DIR=$(dirname "$DIR")
done
[[ -z "$ROOT" ]] && ROOT="$CWD"

JJ_LIST=""
GIT_LIST=""
for SUBDIR in "$ROOT"/*/; do
  [[ -d "$SUBDIR" ]] || continue
  NAME=$(basename "$SUBDIR")
  case "$NAME" in
    node_modules|dist|build|.*) continue ;;
  esac
  if [[ -d "$SUBDIR/.jj" ]]; then
    JJ_LIST+=$'\n'"  - $NAME"
  elif [[ -d "$SUBDIR/.git" || -f "$SUBDIR/.git" ]]; then
    GIT_LIST+=$'\n'"  - $NAME"
  fi
done

[[ -z "$JJ_LIST" && -z "$GIT_LIST" ]] && exit 0

MSG="[vcs-overview] Workspace root: $ROOT"
if [[ -n "$JJ_LIST" ]]; then
  MSG+=$'\n\njj repositories (use jj commands. git mutations are blocked by the git-prefer-jj hook):'"$JJ_LIST"
fi
if [[ -n "$GIT_LIST" ]]; then
  MSG+=$'\n\ngit repositories (use git commands):'"$GIT_LIST"
fi

jq -n --arg ctx "$MSG" \
  '{hookSpecificOutput: {hookEventName: "SessionStart", additionalContext: $ctx}}'

exit 0
