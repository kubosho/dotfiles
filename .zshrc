export LANG=ja_JP.UTF-8
export EDITOR='vim'
export TERM=xterm-256color

##################################################
# import

source ~/.zplug/init.zsh
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
[ -f ~/.travis/travis.sh ] && source ~/.travis/travis.sh

##################################################
# packages

zplug "zsh-users/zsh-completions"
zplug "zplug/zplug"

if ! zplug check; then
  zplug install
fi

zplug load

##################################################
# fpath

fpath=($(brew --prefix)/share/zsh/site-functions $fpath)
fpath=($HOME/.zplug/repos/zsh-users/zsh-completions/src $fpath)

##################################################
# autoload

autoload -Uz compinit
compinit -C # skip to security check

##################################################
# aliases

alias ls="ls -Gv"
alias ll="ls -l"
alias la="ll -a"

alias a="atom"
alias e="emacs"
alias g="git"
alias t="tmux"
alias v="vim"
alias tn="tmuxinator"
alias gco='git-checkout-with-peco'

##################################################
# prompt

autoload -U colors; colors

# もしかして機能
setopt correct

# プロンプトが表示されるたびにプロンプト文字列を評価、置換する
setopt prompt_subst

setopt prompt_subst
autoload -Uz vcs_info
zstyle ':vcs_info:*' actionformats \
    '%F{5}(%f%s%F{5})%F{3}-%F{5}[%F{2}%b%F{3}|%F{1}%a%F{5}]%f '
    zstyle ':vcs_info:*' formats       \
        '%F{5}(%f%s%F{5})%F{3}-%F{5}[%F{2}%b%F{5}]%f '
        zstyle ':vcs_info:(sv[nk]|bzr):*' branchformat '%b%F{1}:%F{3}%r'

        zstyle ':vcs_info:*' enable git cvs svn

# or use pre_cmd, see man zshcontrib
vcs_info_wrapper() {
    vcs_info
      if [ -n "$vcs_info_msg_0_" ]; then
            echo "%{$fg[grey]%}${vcs_info_msg_0_}%{$reset_color%}$del"
              fi
}

# プロンプト指定
PROMPT='
[%n] %{${fg[yellow]}%}%~%{${reset_color}%} %1(v|%F{green}%1v%f|) $(vcs_info_wrapper)
%(?.%{$fg[green]%}.%{$fg[blue]%})%(?!(*'\''-'\'') <!(*;-;%)? <)%{${reset_color}%} '

# プロンプト指定(コマンドの続き)
PROMPT2='[%n]> '

# もしかして時のプロンプト指定
SPROMPT="%{$fg[red]%}%{$suggest%}(*'~'%)? < もしかして %B%r%b %{$fg[red]%}かな? [そう!(y), 違う!(n),a,e]:${reset_color} "

##################################################
# ghq

# http://weblog.bulknews.net/post/89635306479/ghq-peco-percol
function peco-src () {
  local selected_dir=$(ghq list --full-path | peco --query "$LBUFFER")
  if [ -n "$selected_dir" ]; then
      BUFFER="cd ${selected_dir}"
      zle accept-line
  fi
  zle clear-screen
}

function peco-godoc () {
  godoc $(ghq list | peco) | less
}

zle -N peco-src
zle -N peco-godoc
bindkey '^]' peco-src
bindkey '^[' peco-godoc

##################################################
# tmux

source ~/.tmuxinator/tmuxinator.zsh

##################################################
# z

. `brew --prefix`/etc/profile.d/z.sh
function precmd () {
  z --add "$(pwd -P)"
}

##################################################
# save history file

export SAVEHIST=100000
export HISTSIZE=1000
export HISTFILE=${HOME}/.zsh_history
setopt hist_ignore_dups
setopt EXTENDED_HISTORY
