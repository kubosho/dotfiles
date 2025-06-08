# OSC 7 integration for directory tracking
# This sends the current working directory to the terminal when changing directories

function __osc7_precmd() {
  local tmux_prefix=""
  local tmux_suffix=""
  
  # Check if we're in tmux and need passthrough
  if [[ -n "$TMUX" ]]; then
    tmux_prefix="\033Ptmux;\033"
    tmux_suffix="\033\\"
  fi
  
  # Send OSC 7 with current working directory
  printf "${tmux_prefix}\033]7;file://%s%s${tmux_suffix}\033\\" "$(hostname)" "$PWD"
}

# Hook the function to run before each prompt
add-zsh-hook precmd __osc7_precmd