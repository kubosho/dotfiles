export LANG=ja_JP.UTF-8

# ------------------------------
# XDG Base Directory
# ------------------------------
export XDG_BIN_HOME="${HOME}/.local/bin"
export XDG_CACHE_HOME="${HOME}/.cache"
export XDG_CONFIG_HOME="${HOME}/.config"
export XDG_DATA_HOME="${HOME}/.local/share"
export XDG_STATE_HOME="${HOME}/.local/state"

# ------------------------------
# Zsh Configuration
# ------------------------------
export ZSH_CONFIG_DIR="${XDG_CONFIG_HOME}/zsh"

# ------------------------------
# Misc
# ------------------------------
export GOPATH="${XDG_DATA_HOME}"
export VOLTA_HOME="${XDG_CONFIG_HOME}/.volta"

# ------------------------------
# Path
# ------------------------------
path=(
  "${VOLTA_HOME}/bin"
  "/opt/homebrew/bin"
  "${XDG_BIN_HOME}"
  "${HOME}/bin"
  "/usr/local/bin"
  "/usr/local/sbin"
  $path
)
