# zmodload zsh/zprof && zprof

export DENO_INSTALL="$HOME/.deno"
export GOPATH="$HOME"
export INTERACTIVE_FILTERING_TOOL="peco"
export VOLTA_HOME="$HOME/.volta"
export ZSH_CONFIG_DIR="${HOME}/.config/zsh"

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
