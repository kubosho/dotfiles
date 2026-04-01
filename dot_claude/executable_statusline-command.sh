#!/usr/bin/env bash
# ~/.claude/statusline-command.sh
# Claude Code status line: model, context %, git/jj diff stats, rate limit bars

set -euo pipefail

# --------------------------------------------------------------------------
# ANSI color helpers
# --------------------------------------------------------------------------
ansi_rgb() {
  # Usage: ansi_rgb R G B "text"
  printf "\033[38;2;%d;%d;%dm%s\033[0m" "$1" "$2" "$3" "$4"
}

# Palette
GREEN_R=151;  GREEN_G=201;  GREEN_B=195   # #97C9C3
YELLOW_R=229; YELLOW_G=192; YELLOW_B=123  # #E5C07B
RED_R=224;    RED_G=108;    RED_B=117     # #E06C75
GRAY_R=123;   GRAY_G=143;   GRAY_B=150    # #7B8F96

color_for_pct() {
  local pct="$1"
  if   (( pct < 50 )); then echo "$GREEN_R $GREEN_G $GREEN_B"
  elif (( pct < 80 )); then echo "$YELLOW_R $YELLOW_G $YELLOW_B"
  else                      echo "$RED_R $RED_G $RED_B"
  fi
}

colored_pct() {
  local pct="$1"
  read -r r g b <<< "$(color_for_pct "$pct")"
  ansi_rgb "$r" "$g" "$b" "${pct}%"
}

SEP="$(ansi_rgb $GRAY_R $GRAY_G $GRAY_B " │ ")"

# --------------------------------------------------------------------------
# Read stdin JSON
# --------------------------------------------------------------------------
INPUT="$(cat)"

model_display="$(echo "$INPUT" | jq -r '.model.display_name // "Unknown"')"
context_pct_raw="$(echo "$INPUT" | jq -r '.context_window.used_percentage // 0')"
context_pct="$(printf '%.0f' "$context_pct_raw")"
context_size="$(echo "$INPUT" | jq -r '.context_window.context_window_size // 0')"
cwd="$(echo "$INPUT" | jq -r '.workspace.current_dir // .cwd // ""')"

# cost
total_cost="$(echo "$INPUT" | jq -r '.cost.total_cost_usd // 0')"

format_tokens() {
  local n="$1"
  if (( n >= 1000 )); then
    awk "BEGIN{printf \"%.0fk\", ${n}/1000}"
  else
    echo "$n"
  fi
}

# effort level from settings
effort="$(jq -r '.effortLevel // "default"' ~/.claude/settings.json 2>/dev/null || echo "default")"

# --------------------------------------------------------------------------
# Line 1: Model name + effort
# --------------------------------------------------------------------------
LINE1="$(ansi_rgb 217 119 87 "🤖 ${model_display}")${SEP}$(ansi_rgb $GRAY_R $GRAY_G $GRAY_B "🧠 ${effort}")"

# --------------------------------------------------------------------------
# Line 2: context usage + cost
# --------------------------------------------------------------------------
context_used=$(( context_size * context_pct / 100 ))
ctx_display="$(ansi_rgb $GRAY_R $GRAY_G $GRAY_B "📊 ")$(colored_pct "$context_pct")$(ansi_rgb $GRAY_R $GRAY_G $GRAY_B " $(format_tokens $context_used)/$(format_tokens $context_size)")"
cost_str="$(printf '$%.2f' "$total_cost")"
cost_display="$(ansi_rgb 255 215 0 "💰 ${cost_str}")"  # #FFD700 gold

LINE2="${ctx_display}${SEP}${cost_display}"

# --------------------------------------------------------------------------
# Line 3: diff stats + VCS info (jj or git)
# --------------------------------------------------------------------------
added=0; deleted=0; files_changed=0
vcs_info="?"
is_jj=0

if [[ -n "$cwd" ]] && cd "$cwd" 2>/dev/null; then
  # Detect jj repo
  if [[ -d ".jj" ]] || jj root >/dev/null 2>&1; then
    is_jj=1

    # diff stats from jj
    jj_diff="$(jj diff --stat --no-pager 2>/dev/null | tail -1 || true)"
    if [[ -n "$jj_diff" && "$jj_diff" == *"changed"* ]]; then
      files_changed="$(echo "$jj_diff" | grep -oE '[0-9]+ file' | grep -oE '[0-9]+' || echo 0)"
      added="$(echo "$jj_diff" | grep -oE '[0-9]+ insertion' | grep -oE '[0-9]+' || echo 0)"
      deleted="$(echo "$jj_diff" | grep -oE '[0-9]+ deletion' | grep -oE '[0-9]+' || echo 0)"
    fi

    # change ID (shortest)
    change_id="$(jj log -r @ --no-graph -T 'change_id.shortest()' --no-pager 2>/dev/null || echo "?")"

    # bookmarks on current change
    bookmarks="$(jj log -r @ --no-graph -T 'bookmarks' --no-pager 2>/dev/null || true)"

    # working copy status: empty or modified
    wc_empty="$(jj log -r @ --no-graph -T 'if(empty, "empty", "modified")' --no-pager 2>/dev/null || echo "?")"

    # working copy description (first line)
    wc_desc="$(jj log -r @ --no-graph -T 'description.first_line()' --no-pager 2>/dev/null || true)"

    # Build vcs display: 🥋 <change_id> <bookmark> (<status>)
    vcs_colored="🥋 $(ansi_rgb 178 132 190 "$change_id")"  # #B284BE purple for revision
    if [[ -n "$bookmarks" ]]; then
      vcs_colored+=" $(ansi_rgb $GREEN_R $GREEN_G $GREEN_B "$bookmarks")"  # green for bookmark
    fi
    vcs_colored+="$(ansi_rgb $GRAY_R $GRAY_G $GRAY_B " (${wc_empty})")"
  else
    # Fall back to git
    branch="$(git -c core.hooksPath=/dev/null rev-parse --abbrev-ref HEAD 2>/dev/null || echo "?")"
    diff_stat="$(git -c core.hooksPath=/dev/null diff --shortstat HEAD 2>/dev/null || true)"
    if [[ -n "$diff_stat" ]]; then
      files_changed="$(echo "$diff_stat" | grep -oE '[0-9]+ file' | grep -oE '[0-9]+' || echo 0)"
      added="$(echo "$diff_stat" | grep -oE '[0-9]+ insertion' | grep -oE '[0-9]+' || echo 0)"
      deleted="$(echo "$diff_stat" | grep -oE '[0-9]+ deletion' | grep -oE '[0-9]+' || echo 0)"
    fi
    vcs_colored="🐙 $(ansi_rgb $GREEN_R $GREEN_G $GREEN_B "$branch")"  # green for branch
  fi
fi

[[ -z "$added" ]]         && added=0
[[ -z "$deleted" ]]       && deleted=0
[[ -z "$files_changed" ]] && files_changed=0

files_display="$(ansi_rgb $GRAY_R $GRAY_G $GRAY_B "📄 ${files_changed}")"
diff_colored="$(ansi_rgb $GREEN_R $GREEN_G $GREEN_B "+${added}")$(ansi_rgb $GRAY_R $GRAY_G $GRAY_B "/")$(ansi_rgb $RED_R $RED_G $RED_B "-${deleted}")"
diff_colored="✏️ ${diff_colored} ${files_display}"
LINE3="${diff_colored}${SEP}${vcs_colored}"

# --------------------------------------------------------------------------
# Line 4: jj working copy description (jj only)
# --------------------------------------------------------------------------
LINE4=""
if (( is_jj )) && [[ -n "$wc_desc" ]]; then
  LINE4="$(ansi_rgb $GRAY_R $GRAY_G $GRAY_B "💬 ${wc_desc}")"
fi

# --------------------------------------------------------------------------
# Output
# --------------------------------------------------------------------------
printf "%s\n%s\n%s\n" "$LINE1" "$LINE2" "$LINE3"
if [[ -n "$LINE4" ]]; then
  printf "%s\n" "$LINE4"
fi
