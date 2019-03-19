##################################################
# startup

set -gx EDITOR vim

if status --is-login
  # homebrew
  set -gx PATH $PATH /usr/local/bin
  set -gx PATH $PATH /usr/local/sbin

  # my directory
  set -gx PATH $PATH $HOME/bin

  # go
  set -gx GOPATH $HOME
  set -gx PATH $PATH $GOPATH/bin

  # anyenv
  set -x PATH $HOME/.anyenv/bin $PATH
end

##################################################
# aliases

alias a="atom"
alias c="code"
alias e="emacs"
alias g="git"
alias t="tmux"
alias v="vim"
alias tn="tmuxinator"
alias gco='git-checkout-with-peco'

##################################################
# keybind

function fish_user_key_bindings
  bind \c] gh
end

##################################################
# git

set git_dirty_color red
set git_not_dirty_color green

function parse_git_branch
  set -l branch (git branch 2> /dev/null | grep -e '\* ' | sed 's/^..\(.*\)/\1/')
  set -l git_diff (git diff)

  if test -n "$git_diff"
    echo (set_color $git_dirty_color)$branch(set_color normal)
  else
    echo (set_color $git_not_dirty_color)$branch(set_color normal)
  end
end

##################################################
# prompt

function fish_prompt
  if [ $status -eq 0 ]
    set status_face (set_color green)"(*'-') < "
  else
    set status_face (set_color blue)"(*;-;) < "
  end

  set -l git_dir (git rev-parse --git-dir 2> /dev/null)
  set prompt (set_color yellow)(prompt_pwd)

  if test -n "$git_dir"
    echo $prompt [(parse_git_branch)]
    echo $status_face
  else
    echo $prompt
    echo $status_face
  end
end
