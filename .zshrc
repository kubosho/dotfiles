umask 022

export LANG=ja_JP.UTF-8
export EDITOR='vim'
export TERM=xterm-256color

bindkey -e

##################################################
# import

[ -f ~/.travis/travis.sh ] && source ~/.travis/travis.sh

##################################################
# packages

source ~/.zplug/init.zsh

zplug "mafredri/zsh-async"
zplug "sindresorhus/pure"
zplug "zsh-users/zsh-completions"
zplug "zsh-users/zsh-syntax-highlighting"
zplug "zplug/zplug"

if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    fi
fi

zplug load

##################################################
# autoload

autoload -U colors; colors
autoload -Uz vcs_info
autoload -U history-search-end

##################################################
# option

setopt correct
setopt prompt_subst

##################################################
# aliases

alias ls="ls -Gv"
alias ll="ls -l"
alias la="ll -a"

alias a="atom"
alias c="code"
alias e="emacs"
alias g="git"
alias t="tmux"
alias v="vim"
alias bi="bundle install"
alias tn="tmuxinator"
alias rm="trash -i"
alias gco='git-checkout-with-peco'

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
# history

export SAVEHIST=1000000
export HISTSIZE=10000
export HISTFILE=${HOME}/.zsh_history

setopt hist_ignore_dups
setopt share_history

zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^P" history-beginning-search-backward-end
bindkey "^N" history-beginning-search-forward-end

##################################################
# zcompile

if [ $DOTFILES/.zshrc -nt ~/.zshrc.zwc ]; then
  zcompile ~/.zshrc
fi

##################################################
# profile

# if (which zprof > /dev/null 2>&1); then
#   zprof
# fi

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/a14816/google-cloud-sdk/path.zsh.inc' ]; then source '/Users/a14816/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/a14816/google-cloud-sdk/completion.zsh.inc' ]; then source '/Users/a14816/google-cloud-sdk/completion.zsh.inc'; fi
