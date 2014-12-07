##################################################
# path

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"

# homebrew
export PATH=/usr/local/bin:$PATH
export PATH=/usr/local/bin:/usr/bin
export PATH=/usr/local/sbin:$PATH

# my directory
export PATH=$HOME/bin:$PATH

# go
export PATH=$PATH:/usr/local/go/bin
export GOPATH=$HOME
export PATH=$PATH:$GOPATH/bin

# nodebrew
export PATH=$HOME/.nodebrew/current/bin:$PATH

# rbenv
export PATH=$HOME/.rbenv/shims:/usr/local/bin:/usr/bin:/bin:$PATH

# TODO JDKとCabalの設定を吹き飛ばしてしまったので追加しておく
