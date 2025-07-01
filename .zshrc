# ------------------------------
# Zsh Configuration
# ------------------------------

# ref: https://medium.com/@rukurx/zsh%E3%81%AEcompinit%E3%81%AB%E6%8C%87%E5%AE%9A%E3%81%97%E3%81%A6%E3%82%8Bautoload%E3%81%AE%E3%82%AA%E3%83%97%E3%82%B7%E3%83%A7%E3%83%B3-uz-%E3%81%AB%E3%81%A4%E3%81%84%E3%81%A6-ad471efd84c3
autoload -Uz colors && colors
autoload -Uz compinit
autoload -Uz history-search-end
autoload -Uz add-zsh-hook

if [[ -d "$ZSH_CONFIG_DIR" && -r "$ZSH_CONFIG_DIR" ]]; then
  for i in $(find "$ZSH_CONFIG_DIR" -type f -name "*.zsh"); do
    # Only source files under the wsl directory when running on WSL
    if [[ "$i" == */wsl/* ]]; then
      if [ -f /proc/version ] && grep -qEi "(WSL)" /proc/version; then
        [ -r "$i" ] && . "$i"
      fi
    else
      [ -r "$i" ] && . "$i"
    fi
  done
fi

# ------------------------------
# Misc
# ------------------------------

bindkey -e
setopt correct

# ------------------------------
# Aliases
# ------------------------------

alias c="cursor"
alias cc="claude \"Read .claude/CLAUDE.md and .claude/settings.json in the home directory and follow their instructions\""
alias ccusage="deno run -E -R=$HOME/.claude -R=/var/folders/ -R=$XDG_CONFIG_HOME/claude -S=homedir -N='raw.githubusercontent.com:443' --no-prompt npm:ccusage@latest"
alias claude-stream="claude -p --verbose --output-format=stream-json"
alias g="git"
alias t="tmux"
alias v="nvim"

# ------------------------------
# Completion
# ------------------------------

# ref: https://gist.github.com/ctechols/ca1035271ad134841284#gistcomment-2308206
for dump in ~/.zcompdump(N.mh+24); do
  compinit
done

compinit -C

# ref: http://gihyo.jp/dev/serial/01/zsh-book/0005
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z} r:|[-_.]=**'
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' '+m:{A-Z}={a-z}'

# ------------------------------
# History
# ------------------------------

HISTFILE=$HOME/.command_history
HISTSIZE=50000
SAVEHIST=50000

setopt share_history

zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end

# ------------------------------
# Keybind
# ------------------------------

bindkey "^P" history-beginning-search-backward-end
bindkey "^N" history-beginning-search-forward-end

# ------------------------------
# Prompt
# ------------------------------

setopt prompt_subst

PROMPT='
[%n] `show_current_path` `git_current_branch`
`status_code`'
PROMPT2='[%n]> '
SPROMPT='`suggest`'

# ------------------------------
# Sheldon Plugin Manager
# ------------------------------

eval "$(sheldon source)"

# ------------------------------
# Profiling (Must set to end of file)
# ------------------------------

if (which zprof > /dev/null 2>&1) ;then
  zprof
fi
