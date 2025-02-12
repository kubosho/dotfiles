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
# WSL Configuration
# ------------------------------
export USER=$(whoami)
export WINDOWS_VSCODE_PATH="/mnt/c/Users/${USER}/AppData/Local/Programs/Microsoft\ VS\ Code/bin"
export WINDOWS_PATH="/mnt/c/windows"

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
  "${ASDF_DATA_DIR:-$HOME/.asdf}/shims"
  "${VOLTA_HOME}/bin"
  "/opt/homebrew/sbin"
  "/opt/homebrew/bin"
  "${WINDOWS_PATH}"
  "${WINDOWS_VSCODE_PATH}"
  "${XDG_BIN_HOME}"
  "${HOME}/bin"
  "/usr/local/bin"
  "/usr/local/sbin"
  $path
)
