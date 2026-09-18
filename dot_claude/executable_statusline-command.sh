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

colored_bar() {
  local pct="$1" width="$2"
  local filled=$(( pct * width / 100 ))
  (( filled > width )) && filled=$width
  (( filled < 0 )) && filled=0
  local empty=$(( width - filled ))
  local bar="" color
  color="$(color_for_pct "$pct")"
  if (( filled > 0 )); then
    printf -v fill_str "%${filled}s" ""
    bar="$(ansi_rgb ${color} "${fill_str// /█}")"
  fi
  if (( empty > 0 )); then
    printf -v empty_str "%${empty}s" ""
    bar+="$(ansi_rgb $GRAY_R $GRAY_G $GRAY_B "${empty_str// /░}")"
  fi
  printf '%s' "$bar"
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

effort="$(echo "$INPUT" | jq -r '.effort.level // "default"')"
total_cost="$(echo "$INPUT" | jq -r '.cost.total_cost_usd // 0')"

# --------------------------------------------------------------------------
# Line 1: Model name + effort
# --------------------------------------------------------------------------
LINE1="$(ansi_rgb 217 119 87 "🤖 ${model_display}") $(ansi_rgb $GRAY_R $GRAY_G $GRAY_B "🧠 ${effort}")"

# --------------------------------------------------------------------------
# Line 2: context usage + cost
# --------------------------------------------------------------------------
context_used="$(echo "$INPUT" | jq -r '.context_window.total_input_tokens // 0')"

compact_threshold="${CLAUDE_AUTOCOMPACT_PCT_OVERRIDE:-}"
if [[ -z "$compact_threshold" ]]; then
  compact_threshold="$(jq -r '.env.CLAUDE_AUTOCOMPACT_PCT_OVERRIDE // empty' ~/.claude/settings.json 2>/dev/null || true)"
fi

if [[ -n "$compact_threshold" ]] && (( compact_threshold > 0 )); then
  # color reflects distance to auto-compact, not the full context window
  compact_ceiling=$(( context_size * compact_threshold / 100 ))
  if (( compact_ceiling > 0 )); then
    pct_to_compact=$(( context_used * 100 / compact_ceiling ))
  else
    pct_to_compact=0
  fi
  ctx_bar="$(colored_bar "$pct_to_compact" 10)"
  ctx_display="$(ansi_rgb $GRAY_R $GRAY_G $GRAY_B "📊 ")${ctx_bar} $(colored_pct "$pct_to_compact")"
else
  ctx_bar="$(colored_bar "$context_pct" 10)"
  ctx_display="$(ansi_rgb $GRAY_R $GRAY_G $GRAY_B "📊 ")${ctx_bar} $(colored_pct "$context_pct")"
fi
cost_str="$(printf '$%.2f' "$total_cost")"
cost_display="$(ansi_rgb 255 215 0 "💰 ${cost_str}")"  # #FFD700 gold

LINE2="${ctx_display}${SEP}${cost_display}"

# --------------------------------------------------------------------------
# Line 3: rate limits
# --------------------------------------------------------------------------
# rate_limits is absent until the session's first API response. The limits are
# account-wide rather than per-session, so the last values any session saw are
# still accurate: cache them and fall back to the cache while the field is missing.
RL_CACHE="${XDG_CACHE_HOME:-$HOME/.cache}/claude/statusline-rate-limits.json"

rl_json="$(echo "$INPUT" | jq -c '.rate_limits // empty')"
if [[ -n "$rl_json" ]]; then
  rl_tmp="${RL_CACHE}.$$"
  { mkdir -p "${RL_CACHE%/*}" && printf '%s' "$rl_json" > "$rl_tmp" && mv "$rl_tmp" "$RL_CACHE"; } 2>/dev/null \
    || rm -f "$rl_tmp" 2>/dev/null || true
else
  # Each window resets on its own schedule, so drop them one by one: a cached
  # value stays the latest known one until its own window resets.
  rl_json="$(jq -c --argjson now "$(date +%s)" \
    'with_entries(select(.value.resets_at > $now))' "$RL_CACHE" 2>/dev/null || true)"
fi

rl_5h_pct="$(echo "$rl_json" | jq -r '.five_hour.used_percentage // empty' 2>/dev/null || true)"
rl_7d_pct="$(echo "$rl_json" | jq -r '.seven_day.used_percentage // empty' 2>/dev/null || true)"

rl_window() {
  # Usage: rl_window <label> <percentage or empty>
  # A window with no usable value shows a placeholder, keeping the line and the
  # other window in place instead of letting them disappear.
  local label="$1" pct="$2" rounded color
  if [[ -z "$pct" ]]; then
    printf '%s %s %s' \
      "$(ansi_rgb $GRAY_R $GRAY_G $GRAY_B "${label}:")" \
      "$(colored_bar 0 8)" \
      "$(ansi_rgb $GRAY_R $GRAY_G $GRAY_B "--%")"
    return
  fi
  rounded="$(printf '%.0f' "$pct")"
  color="$(color_for_pct "$rounded")"
  printf '%s %s %s' \
    "$(ansi_rgb ${color} "${label}:")" \
    "$(colored_bar "$rounded" 8)" \
    "$(ansi_rgb ${color} "${rounded}%")"
}

LINE3="$(ansi_rgb $GRAY_R $GRAY_G $GRAY_B "⏳ ")$(rl_window 5h "$rl_5h_pct")${SEP}$(rl_window 7d "$rl_7d_pct")"

# --------------------------------------------------------------------------
# Line 4: diff stats + VCS info + commit message (jj or git)
# --------------------------------------------------------------------------
added=0; deleted=0; files_changed=0
vcs_colored="?"
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
LINE4="${diff_colored}${SEP}${vcs_colored}"
if (( is_jj )) && [[ -n "$wc_desc" ]]; then
  LINE4+="${SEP}$(ansi_rgb $GRAY_R $GRAY_G $GRAY_B "💬 ${wc_desc}")"
fi

# --------------------------------------------------------------------------
# Output
# --------------------------------------------------------------------------
printf "%s\n%s\n%s\n" "$LINE1" "$LINE2" "$LINE3"
printf "%s\n" "$LINE4"
