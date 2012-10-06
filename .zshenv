##################################################
# path

## 重複したパスを登録しない。
typeset -U path
typeset -U sudo_path

path=(
    ## my directory
    $HOME/local/bin(N-/)

    ## homebrew
    /usr/local/bin(N-/)
    /usr/local/sbin(N-/)

    ## system
    /usr/bin(N-/)
    /usr/sbin(N-/)
    /bin(N-/)
    /sbin(N-/)
    /usr/X11/bin(N-/)
)

## sudo
sudo_path=({,/usr/pkg,/usr/local,/usr}/sbin(N-/))

## -x: export SUDO_PATHも一緒に行う。
## -T: SUDO_PATHとsudo_pathを連動する。
typeset -xT SUDO_PATH sudo_path

export PATH=/usr/local/Cellar/php/5.3.10/bin:$PATH

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting

# nvm と指定されたバージョンの Node.js がインストール済みの場合だけ
# 設定を有効にする
if [[ -f ~/.nvm/nvm.sh ]]; then
  source ~/.nvm/nvm.sh

  if which nvm >/dev/null 2>&1 ;then
    _nodejs_use_version="v0.8.11"
    if nvm ls | grep -F -e "${_nodejs_use_version}" >/dev/null 2>&1 ;then
      nvm use "${_nodejs_use_version}" >/dev/null
      export NODE_PATH=${NVM_PATH}_modules${NODE_PATH:+:}${NODE_PATH}
    fi
    unset _nodejs_use_version
  fi
fi
