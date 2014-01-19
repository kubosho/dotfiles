export LANG=ja_JP.UTF-8

##################################################
# aliases

alias ls="ls -Gv"
alias ll="ls -l"
alias la="ll -a"

alias app="open -a"

alias g="git"

alias e="emacs"
alias v="vim"

##################################################
# path

# 重複したパスを登録しない。
typeset -U path
typeset -U sudo_path

# homebrew
export PATH=/usr/local/bin:$PATH
export PATH=/usr/local/sbin:$PATH

# my directory
export PATH=$HOME/local/bin:$PATH

# nodebrew
if [[ -f ~/.nodebrew/nodebrew ]]; then
  export PATH=$HOME/.nodebrew/current/bin:$PATH
fi
