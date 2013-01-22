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
compinit -u

## http://d.hatena.ne.jp/guyon/20120116/1326725427
zstyle ':completion:*:sudo:*' command-path /usr/local/sbin /usr/local/bin \
                             /usr/sbin /usr/bin /sbin /bin /usr/X11R6/bin \
                             /usr/local/git/bin

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

autoload -U colors; colors

setopt correct

autoload -Uz VCS_INFO_get_data_git; VCS_INFO_get_data_git 2> /dev/null

function prompt-git-current-branch {
    local name st color gitdir action
    if [[ "$PWD" =~ '/\.git(/.*)?$' ]]; then
        return
    fi
    name=`git rev-parse --abbrev-ref=loose HEAD 2> /dev/null`
    if [[ -z $name ]]; then
        return
    fi

    gitdir=`git rev-parse --git-dir 2> /dev/null`
    action=`VCS_INFO_git_getaction "$gitdir"` && action="($action)"

    st=`git status 2> /dev/null`
    if [[ -n `echo "$st" | grep "^nothing to"` ]]; then
        color=%F{green}
    elif [[ -n `echo "$st" | grep "^no changes added"` ]]; then
        color=%F{yellow}
    elif [[ -n `echo "$st" | grep "^# Changes to be committed"` ]]; then
        color=%B%F{red}
    else
        color=%F{red}
    fi

    echo "[$color$name$action%f%b]"
}

# PCRE 互換の正規表現を使う
setopt re_match_pcre

# プロンプトが表示されるたびにプロンプト文字列を評価、置換する
setopt prompt_subst

PROMPT="
[%n] %{${fg[yellow]}%}%~%{${reset_color}%}
%(?.%{$fg[green]%}.%{$fg[blue]%})%(?!(＠ﾟ□ﾟ)ノ <!ZzZz(＠￣￢￣%)ノ <)%{${reset_color}%} "

PROMPT2='[%n]> '

RPROMPT='`prompt-git-current-branch`'

SPROMPT="%{$fg[red]%}%{$suggest%}(＠ﾟ△ﾟ%)ノ < もしかして %B%r%b %{$fg[red]%}かな? [そう!(y), 違う!(n),a,e]:${reset_color} "

##################################################
# etc

## Emacsキーバインドを使う。
bindkey -e

## 実行したプロセスの消費時間が3秒以上かかったら
## 自動的に消費時間の統計情報を表示する。
REPORTTIME=3

## create emacs env file
perl -wle \
    'do { print qq/(setenv "$_" "$ENV{$_}")/ if exists $ENV{$_} } for @ARGV' \
    PATH > ~/.emacs.d/shellenv.el

## Googleで検索
function google() {
  local str opt
  if [ $# != 0 ]; then
    for i in $*; do
      str="$str+$i"
    done
    str=`echo $str | sed 's/^\+//'`
    opt='search?num=50&hl=ja&lr=lang_ja'
    opt="${opt}&q=${str}"
  fi
  open -a Google\ Chrome http://www.google.co.jp/$opt
}
