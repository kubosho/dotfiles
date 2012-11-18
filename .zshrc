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
# change directory

## ディレクトリ名だけでcdする。
setopt auto_cd

## cdで移動してもpushdと同じようにディレクトリスタックに追加する。
setopt auto_pushd

## カレントディレクトリ中に指定されたディレクトリが見つからなかった場合に
## 移動先を検索するリスト。
cdpath=(~)

## ディレクトリが変わったらディレクトリスタックを表示。
chpwd_functions=($chpwd_functions dirs)

## ヒストリを保存するファイル
HISTFILE=~/.zsh_history

##################################################
# complement

## 補完
autoload -Uz compinit
compinit

## 補完候補に色を付ける。
### "": 空文字列はデフォルト値を使うという意味。
zstyle ':completion:*:default' list-colors ""

## 補完候補がなければより曖昧に候補を探す。
### m:{a-z}={A-Z}: 小文字を大文字に変えたものでも補完する。
### r:|[._-]=*: 「.」「_」「-」の前にワイルドカード「*」があるものとして補完する。
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z} r:|[._-]=*'

## 補完候補をキャッシュする。
zstyle ':completion:*' use-cache yes

## 補完時にヒストリを自動的に展開する。
setopt hist_expand

## 補完候補がないときなどにビープ音を鳴らさない。
setopt no_beep

##################################################
# color

export LS_COLORS='di=36:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'

##################################################
# Editor

export EDITOR=/Applications/MacVim.app/Contents/MacOS/Vim
alias vi='env LANG=ja_JP.UTF-8 /Applications/MacVim.app/Contents/MacOS/Vim "$@"'
alias vim='env LANG=ja_JP.UTF-8 /Applications/MacVim.app/Contents/MacOS/Vim "$@"'

##################################################
# history

## メモリ上の履歴数。
## 大きな数を指定してすべての履歴を保存するようにしている。
HISTSIZE=10000000

## 保存する履歴数
SAVEHIST=$HISTSIZE

## 履歴ファイルにコマンドだけではなく実行時刻と実行時間も保存する。
setopt extended_history

## 重複した履歴を保存しない
setopt hist_ignore_dups

## スペースで始まるコマンドは履歴に追加しない。
setopt hist_ignore_space

## すぐに履歴ファイルに追記する。
setopt inc_append_history

## zshプロセス間で履歴を共有する。
setopt share_history

## C-sでの履歴検索が潰されてしまうため、出力停止・開始用にC-s/C-qを使わない。
setopt no_flow_control

## 履歴の検索
autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^P" history-beginning-search-backward-end
bindkey "^N" history-beginning-search-forward-end

##################################################
# prompt

autoload colors
colors

setopt correct

PROMPT="
%{${fg[yellow]}%}%~%{${reset_color}%}
%(?.%{$fg[green]%}.%{$fg[blue]%})%(?!(*'-') <!(*;-;%)? <)%{${reset_color}%} "

PROMPT2='[%n]> '

SPROMPT="%{$fg[red]%}%{$suggest%}(*'~'%)? < もしかして %B%r%b %{$fg[red]%}かな? [そう!(y), 違う!(n),a,e]:${reset_color} "

##################################################
# etc

## Emacsキーバインドを使う。
bindkey -e

## 実行したプロセスの消費時間が3秒以上かかったら
## 自動的に消費時間の統計情報を表示する。
REPORTTIME=3

## perlbrew
source ~/perl5/perlbrew/etc/bashrc

## create emacs env file
perl -wle \
    'do { print qq/(setenv "$_" "$ENV{$_}")/ if exists $ENV{$_} } for @ARGV' \
    PATH > ~/.emacs.d/shellenv.el

##################################################
# rvm

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
source "$HOME/.rvm/scripts/rvm"

##################################################
# vim

export VIMHOME=$HOME/.vim

##################################################
# phpenv
if [ -f ${HOME}/.phpenv/bin/phpenv ]; then
    export PATH=${PATH}:${HOME}/.phpenv/bin
    eval "$(phpenv init -)"
fi
