# references:
# [tmux] 端末起動時に自動で新規セッションを作成 or 既存セッションにアタッチ #Linux - Qiita
# https://qiita.com/ssh0/items/a9956a74bff8254a606a

if [[ ! -n $TMUX && $- == *l* ]]; then
  # get the IDs
  ID="`tmux list-sessions`"
  if [[ -z "$ID" ]]; then
    tmux new-session
  fi
  create_new_session="Create New Session"
  ID="$ID\n${create_new_session}:"
  ID="`echo $ID | $INTERACTIVE_FILTERING_TOOL | cut -d: -f1`"
  if [[ "$ID" = "${create_new_session}" ]]; then
    tmux new-session
  elif [[ -n "$ID" ]]; then
    tmux attach-session -t "$ID"
  else
    : # Start terminal normally
  fi
fi
