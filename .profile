export LANG=en_US.UTF-8
export LC_ALL=$LANG
export DISPLAY=:0.0
export LIBGL_ALWAYS_INDIRECT=1
export GOPATH=$HOME
export WINDOWS_HOME=/mnt/c/Users/kubosho
export PATH="$HOME/bin:$HOME/.local/bin:/usr/local/bin:/usr/bin:/bin:PATH"


if [ -n "$BASH_VERSION" ]; then
  if [ -f $HOME/.bashrc ] ; then
    . $HOME/.bashrc
  fi
fi
export VOLTA_HOME="$HOME/.volta"
export PATH="$VOLTA_HOME/bin:$PATH"
