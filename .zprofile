##################################################
# path

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"

# homebrew
export PATH=/usr/local/bin:$PATH
export PATH=/usr/local/sbin:$PATH
export PATH=/bin:$PATH
export PATH=/sbin:$PATH

# my directory
export PATH=$HOME/bin:$PATH

# go
export PATH=$PATH:/usr/local/go/bin
export GOPATH=$HOME
export PATH=$PATH:$GOPATH/bin

# python
export PATH=$HOME/Library/Python/2.7/bin:$PATH

# Cabal
export PATH=$HOME/Library/Haskell/bin:$PATH

# JDK
export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.8.0_25.jdk/Contents/Home

# redpen
export PATH=/usr/local/redpen/bin:$PATH

# nodebrew
export PATH=$HOME/.nodebrew/current/bin:$PATH

# app engine
export PATH=$HOME/go_appengine:$PATH

# prott
export PATH=$HOME/src/github.com/goodpatch/prott-commands/bin:$PATH

# PostgreSQL Settings
export ARCHFLAGS="-arch x86_64"
export PGDATA="/usr/local/var/postgres"
