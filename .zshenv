# zmodload zsh/zprof && zprof

export ANYENV_ROOT="$HOME/.anyenv"
export DOCKER_HOST="tcp://0.0.0.0:2375"
export GOPATH="$HOME"
export ZSH_CONFIG_DIR="${HOME}/.zshconfig/zsh"

# ref: https://qiita.com/vintersnow/items/7343b9bf60ea468a4180#anyenv-%E9%81%85%E5%BB%B6%E3%83%AD%E3%83%BC%E3%83%89
if [ -d $ANYENV_ROOT ]; then
  export PATH="$ANYENV_ROOT/bin:$PATH"

  for D in `command ls $ANYENV_ROOT/envs`
  do
    export PATH="$ANYENV_ROOT/envs/$D/shims:$PATH"
  done
fi

# ref: https://qiita.com/GeneralD/items/a87d5d145149a9d214c6
path=(
  "/usr/local/bin"
  "/usr/local/sbin"
  "/opt/homebrew/bin"
  "$HOME/.homebrew/bin"
  "$HOME/bin"
  $path
)
