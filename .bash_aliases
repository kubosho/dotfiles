# for WSL
if [ -f /proc/sys/fs/binfmt_misc/WSLInterop ]; then
  alias code='/mnt/c/Program\ Files/Microsoft\ VS\ Code/Code.exe'
fi

alias c='code'
alias e='emacs'
alias g='git'
alias v='vim'

alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
  test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
  alias ls='ls --color=auto'

  alias grep='grep --color=auto'
  alias fgrep='fgrep --color=auto'
  alias egrep='egrep --color=auto'
fi
