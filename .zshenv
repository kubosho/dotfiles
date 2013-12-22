export LANG=ja_JP.UTF-8

##################################################
# aliases

alias ls="ls -Gv"
alias ll="ls -l"
alias la="ll -a"

alias app="open -a"

alias g="git"
alias s="svn"

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
export PATH=$HOME/.nodebrew/node/v0.10.5/lib/node_modules:$PATH

# gem
export PATH=$HOME/.gem/ruby/1.9.1/bin:$PATH
export PATH=/usr/local/Cellar/ruby/2.0.0-p247/bin:$PATH

# perlbrew
source $HOME/perl5/perlbrew/etc/bashrc
