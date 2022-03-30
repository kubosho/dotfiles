function load-nvm () {
  export NVM_DIR="$HOME/.nvm"
  [[ -s $(brew --prefix nvm)/nvm.sh ]] && source $(brew --prefix nvm)/nvm.sh
}

load-nvmrc() {
  if [[ -f .nvmrc && -r .nvmrc ]]; then
    if ! type nvm >/dev/null; then
      load-nvm
    fi
    nvm use
  fi
}

add-zsh-hook chpwd load-nvmrc
