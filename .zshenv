##################################################
# aliases

alias ls="ls -G"
alias ll="ls -l"
alias la="ll -a"

alias app="open -a"

alias g="git"
alias s="svn"
alias ta="tmux attach"

alias emacs="/Applications/Emacs.app/Contents/MacOS/Emacs -nw"
alias e="emacs"

alias vim="/Applications/MacVim.app/Contents/MacOS/Vim '$@'"
alias vi="/Applications/MacVim.app/Contents/MacOS/Vim '$@'"
alias v="vim"

alias oe="open -a Emacs"
alias ov="open -a MacVim"

##################################################
# path

# fpath=(~/.zsh-completions $fpath)

# 重複したパスを登録しない。
typeset -U path
typeset -U sudo_path

# homebrew
export PATH=/usr/local/bin:$PATH
export PATH=/usr/local/sbin:$PATH

# my directory
export PATH=$HOME/local/bin:$PATH

# node.js
export PATH=$HOME/.nodebrew/current/bin:$PATH
export PATH=$HOME/.nodebrew/node/v0.8.14/lib/node_modules:$PATH

# rvm
export PATH=$HOME/.rvm/bin:$PATH

# gems
export GEM_HOME=$HOME/.rvm/gems/ruby-1.9.3-p194/gems

# use rvm
[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm"
