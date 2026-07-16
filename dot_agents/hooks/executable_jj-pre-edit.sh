#!/bin/bash
# PreToolUse hook for file-modification tools (Edit/Write)

set -euo pipefail

# exit 0 = skip hook silently
# Conditions: jq/jj not installed, no file path in input, edited file not in
# a jj repo, or the target repo's working copy has no description yet
command -v jq >/dev/null 2>&1 || exit 0
command -v jj >/dev/null 2>&1 || exit 0

INPUT=$(cat)
FILE_PATH=$(echo "$INPUT" | jq -r '.tool_input.file_path // empty')
[[ -n "$FILE_PATH" ]] || exit 0

# Write may create the file and its parent directories, so climb to the
# nearest existing ancestor before asking jj which repo owns the path
DIR=$(dirname "$FILE_PATH")
while [[ ! -d "$DIR" ]]; do
  DIR=$(dirname "$DIR")
done
cd "$DIR" 2>/dev/null || exit 0

# Repo discovery is delegated to `jj root` rather than a manual ancestor
# search for .jj. The upward search rules live in jj and a reimplementation
# would drift when they change.
jj root >/dev/null 2>&1 || exit 0

DESC=$(jj log --no-graph -r @ -T 'description.first_line()' 2>/dev/null || echo "")
[[ -z "$DESC" ]] && exit 0

jq -n --arg ctx "[jj-workflow] The working copy of the repository containing the file you are editing is described as \"${DESC}\". A commit should carry one motivation. If the change you are about to write belongs to a different motivation, split now. Same motivation: proceed. Different motivation: run jj new first." \
  '{hookSpecificOutput: {hookEventName: "PreToolUse", additionalContext: $ctx}}'

exit 0
