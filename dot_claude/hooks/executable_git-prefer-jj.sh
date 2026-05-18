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

# Require git to start a command segment: at the start of input or after a
# shell separator (;, |, &, opening subshell paren), optionally followed by
# whitespace. Plain whitespace alone does not count, so trigger substrings
# inside quoted arguments (e.g. a jj split -m message) are not blocked.
GIT_LEAD='(^|[;|&(][[:space:]]*)git[[:space:]]+'

if [[ "$COMMAND" =~ ${GIT_LEAD}(checkout[[:space:]]+-b|switch[[:space:]]+-c|branch[[:space:]]) ]]; then
  cat >&2 <<'EOF'
[jj-workflow] Target directory is a jj repo. Use jj instead of the git CLI for branch or commit creation:

  jj new <base>                    # create a new working copy on <base> (default: main)
  jj bookmark create <name> -r @   # create a bookmark on the current revision

If you intentionally need git, state your reason and retry.
EOF
  exit 2
elif [[ "$COMMAND" =~ ${GIT_LEAD}commit([[:space:]]|$) ]]; then
  cat >&2 <<'EOF'
[jj-workflow] Target directory is a jj repo. Use jj instead of the git CLI for commits:

  jj describe -m "type: summary"   # describe the current working copy
  jj new -m "type: summary"        # create a new commit on top and describe it

If you intentionally need git, state your reason and retry.
EOF
  exit 2
fi

exit 0
