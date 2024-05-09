# zmodload zsh/zprof && zprof

export DENO_INSTALL="$HOME/.deno"
export GOPATH="$HOME"
export VOLTA_HOME="$HOME/.volta"
export ZSH_CONFIG_DIR="${HOME}/.zshconfig"

# ref: https://qiita.com/GeneralD/items/a87d5d145149a9d214c6
path=(
  "/usr/local/bin"
  "/usr/local/sbin"
  "/opt/homebrew/bin"
  "$HOME/.homebrew/bin"
  "$HOME/bin"
  "$HOME/.nodebrew/current/bin"
  "$DENO_INSTALL/bin"
  "$VOLTA_HOME/bin:$PATH"
  $path
)
