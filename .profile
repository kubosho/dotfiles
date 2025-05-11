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
# Homebrew on WSL
# ------------------------------
if grep -qEi "(WSL)" /proc/version; then
  # Set PATH, MANPATH, etc., for Homebrew.
  eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
fi

# ------------------------------
# Keychain on WSL
# ------------------------------
if grep -qEi "(WSL)" /proc/version; then
  source $HOME/.keychain/$HOST-sh
fi

# ------------------------------
# Runtime
# ------------------------------
if [ -f ${XDG_BIN_HOME}/mise ]; then
  eval "$(${XDG_BIN_HOME}/mise activate zsh)"
fi

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
  "${XDG_CACHE_HOME}/.bun/bin"
  "${VOLTA_HOME}/bin"
)

# for macOS
if [ "$(uname)" = "Darwin" ]; then
  path+=("/opt/homebrew/sbin")
  path+=("/opt/homebrew/bin")
fi

# for WSL
if grep -qEi "(WSL)" /proc/version; then
  path+=("${WINDOWS_PATH}")
  path+=("${WINDOWS_CURSOR_PATH}")
  path+=("${WINDOWS_VSCODE_PATH}")
fi

path=(
  "${XDG_BIN_HOME}"
  "${HOME}/bin"
  "/usr/local/bin"
  "/usr/local/sbin"
  $path
)

export PATH=$(IFS=:; echo "${path[*]}")
