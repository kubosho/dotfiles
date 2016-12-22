if status --is-login
  # homebrew
  set PATH $PATH /usr/local/bin
  set PATH $PATH /usr/local/sbin

  # my directory
  set PATH $PATH $HOME/bin

  # go
  set GOPATH $HOME
  set PATH $PATH $GOPATH/bin

  # prott
  set PATH $PATH $HOME/src/github.com/goodpatch/prott-commands/bin

  set PATH $PATH /usr/bin /sbin
end

function fish_user_key_bindings
  bind \c] peco_select_ghq_repository
end

