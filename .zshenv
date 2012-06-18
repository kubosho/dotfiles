##################################################
# path

## 重複したパスを登録しない。
typeset -U path
typeset -U sudo_path

path=(
    ## my directory
    $HOME/local/bin(N-/)

    ## homebrew
    /usr/local/bin(N-/)
    /usr/local/sbin(N-/)

    ## system
    /usr/bin(N-/)
    /usr/sbin(N-/)
    /bin(N-/)
    /sbin(N-/)
    /usr/X11/bin(N-/)
)

## sudo
sudo_path=({,/usr/pkg,/usr/local,/usr}/sbin(N-/))

## -x: export SUDO_PATHも一緒に行う。
## -T: SUDO_PATHとsudo_pathを連動する。
typeset -xT SUDO_PATH sudo_path

export PATH=/usr/local/Cellar/php/5.3.10/bin:$PATH

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
