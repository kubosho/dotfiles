#!/bin/bash
# PreToolUse hook for Bash tool
# Block git mutation commands when the target directory has .jj. Suggest the jj equivalent.

set -euo pipefail

command -v jq >/dev/null 2>&1 || exit 0

INPUT=$(cat)
COMMAND=$(echo "$INPUT" | jq -r '.tool_input.command // ""')
CWD=$(echo "$INPUT" | jq -r '.cwd')

# Resolve target directory from `cd <path>` or `git -C <path>` if present.
TARGET_DIR="$CWD"
if [[ "$COMMAND" =~ cd[[:space:]]+([^[:space:]\&\;\|]+) ]]; then
  CANDIDATE="${BASH_REMATCH[1]}"
  [[ "$CANDIDATE" = /* ]] && TARGET_DIR="$CANDIDATE" || TARGET_DIR="$CWD/$CANDIDATE"
elif [[ "$COMMAND" =~ git[[:space:]]+-C[[:space:]]+([^[:space:]]+) ]]; then
  CANDIDATE="${BASH_REMATCH[1]}"
  [[ "$CANDIDATE" = /* ]] && TARGET_DIR="$CANDIDATE" || TARGET_DIR="$CWD/$CANDIDATE"
fi

[[ -d "$TARGET_DIR/.jj" ]] || exit 0

case "$COMMAND" in
  *"git checkout -b"*|*"git switch -c"*|*"git branch "*)
    cat >&2 <<'EOF'
[jj-workflow] Target directory is a jj repo. Use jj instead of git for branch/commit creation:

  jj new <base>                    # create a new working copy on <base> (default: main)
  jj bookmark create <name> -r @   # create a bookmark on the current revision

If you intentionally need git, state your reason and retry.
EOF
    exit 2
    ;;
  *"git commit"*)
    cat >&2 <<'EOF'
[jj-workflow] Target directory is a jj repo. Use jj instead of git commit:

  jj describe -m "type: summary"   # describe the current working copy
  jj new -m "type: summary"        # create a new commit on top and describe it

If you intentionally need git, state your reason and retry.
EOF
    exit 2
    ;;
esac

exit 0
