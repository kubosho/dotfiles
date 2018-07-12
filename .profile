export LANG=en_US.UTF-8
export LC_ALL=$LANG
export DISPLAY=localhost:0.0
export PATH="$HOME/bin:$HOME/.local/bin:/usr/local/bin:/usr/bin:/bin:PATH"
export GOPATH=$HOME
export WINDOWS_HOME=/mnt/c/Users/ta2

if [ -n "$BASH_VERSION" ]; then
  if [ -f $HOME/.bashrc ] ; then
    . $HOME/.bashrc
  fi
fi
