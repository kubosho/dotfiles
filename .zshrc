export LANG=ja_JP.UTF-8

# ref: https://medium.com/@rukurx/zsh%E3%81%AEcompinit%E3%81%AB%E6%8C%87%E5%AE%9A%E3%81%97%E3%81%A6%E3%82%8Bautoload%E3%81%AE%E3%82%AA%E3%83%97%E3%82%B7%E3%83%A7%E3%83%B3-uz-%E3%81%AB%E3%81%A4%E3%81%84%E3%81%A6-ad471efd84c3
autoload -Uz colors && colors
autoload -Uz compinit
autoload -Uz history-search-end
autoload -Uz add-zsh-hook

## Homebrew

if [ "$(uname)" = 'Linux' ]; then
  # Set PATH, MANPATH, etc., for Homebrew.
  eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
fi

# ref: http://fnwiya.hatenablog.com/entry/2015/11/03/191902
if [ -d $ZSH_CONFIG_DIR -a -r $ZSH_CONFIG_DIR -a \
   -x $ZSH_CONFIG_DIR ]; then
  for i in $ZSH_CONFIG_DIR/*; do
    [[ ${i##*/} = *.sh || ${i##*/} = *.zsh ]] &&
      [ \( -f $i -o -h $i \) -a -r $i ] && . $i
  done
fi

## Misc

bindkey -e
setopt correct

## Aliases

alias a="atom"
alias c="code"
alias e="emacs"
alias g="git"
alias t="tmux"
alias v="vim"

## Completion

# ref: https://gist.github.com/ctechols/ca1035271ad134841284#gistcomment-2308206
for dump in ~/.zcompdump(N.mh+24); do
  compinit
done

compinit -C

# ref: http://gihyo.jp/dev/serial/01/zsh-book/0005
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z} r:|[-_.]=**'
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' '+m:{A-Z}={a-z}'

## History

HISTFILE=$HOME/.command_history
HISTSIZE=50000
SAVEHIST=50000

setopt share_history

zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end

## Keybind

bindkey "^P" history-beginning-search-backward-end
bindkey "^N" history-beginning-search-forward-end

## Prompt

setopt prompt_subst

PROMPT='
[%n] `show_current_path` `git_current_branch`
`status_code`'
PROMPT2='[%n]> '
SPROMPT='`suggest`'

## Profiling (Must set to end of file)

if (which zprof > /dev/null 2>&1) ;then
  zprof
fi
