##################################################
# import

. $HOME/.config/fisherman/z-fish/z.fish

##################################################
# startup

if status --is-login
  # homebrew
  set PATH $PATH /usr/local/bin
  set PATH $PATH /usr/local/sbin

  # my directory
  set PATH $PATH $HOME/bin

  # go
  set GOPATH $HOME
  set PATH $PATH $GOPATH/bin

  # anyenv
  set PATH $PATH $HOME/.anyenv/bin

  for dir in (ls $HOME/.anyenv/envs)
    set PATH $PATH $HOME/.anyenv/envs/$dir/bin
    set PATH $PATH $HOME/.anyenv/envs/$dir/shims
  end

  . $HOME/.anyenv/completions/anyenv.fish
end

##################################################
# aliases

alias a="atom"
alias e="emacs"
alias g="git"
alias t="tmux"
alias v="vim"
alias tn="tmuxinator"
alias gco='git-checkout-with-peco'

##################################################
# keybind

function fish_user_key_bindings
  bind \c] peco_select_ghq_repository
end

##################################################
# git

set fish_git_dirty_color red
set fish_git_not_dirty_color green

function parse_git_branch
  set -l branch (git branch 2> /dev/null | grep -e '\* ' | sed 's/^..\(.*\)/\1/')
  set -l git_diff (git diff)

  if test -n "$git_diff"
    echo (set_color $fish_git_dirty_color)$branch(set_color normal)
  else
    echo (set_color $fish_git_not_dirty_color)$branch(set_color normal)
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

