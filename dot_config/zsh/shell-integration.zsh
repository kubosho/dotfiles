# Load WezTerm shell integration
# This provides OSC 7 and other terminal integration features

# Only load if not already loaded
if [[ -z "$WEZTERM_SHELL_INTEGRATION" ]]; then
  local integration_file="$HOME/.config/wezterm/scripts/wezterm.sh"
  
  if [[ -f "$integration_file" ]]; then
    source "$integration_file"
  fi
fi