##################################################
# path

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"

# homebrew
export PATH=/usr/local/bin:$PATH
export PATH=/usr/local/sbin:$PATH

# my directory
export PATH=$HOME/bin:$PATH

# go
export GOPATH=$HOME/go
export GOROOT=/opt/boxen/homebrew/opt/go/libexec
export PATH=$GOPATH/bin:$PATH
export PATH=$GOROOT/bin:$PATH

# nodebrew
if [[ -f ~/.nodebrew/nodebrew ]]; then
  export PATH=$HOME/.nodebrew/current/bin:$PATH
fi

