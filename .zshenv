# zmodload zsh/zprof && zprof

export DENO_INSTALL="$HOME/.deno"
export GOPATH="$HOME"
export INTERACTIVE_FILTERING_TOOL="peco"
export VOLTA_HOME="$HOME/.volta"
export ZSH_CONFIG_DIR="${HOME}/.config/zsh"

export XDG_BIN_HOME="${HOME}/.local/bin"
export XDG_CACHE_HOME="${HOME}/.cache"
export XDG_CONFIG_HOME="${HOME}/.config"
export XDG_DATA_HOME="${HOME}/.local/share"
export XDG_STATE_HOME="${HOME}/.local/state"

# ref: https://qiita.com/GeneralD/items/a87d5d145149a9d214c6
path=(
  "/usr/local/bin"
  "/usr/local/sbin"
  "$HOME/bin"
  "$DENO_INSTALL/bin"
  "$VOLTA_HOME/bin:$PATH"
  "/opt/homebrew/bin"
  "$HOME/.homebrew/bin"
  $path
)
