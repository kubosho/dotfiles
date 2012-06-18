#!/bin/sh

# 以下スクリプトをそのまま使用
# https://github.com/sugyan/dotfiles/blob/master/create_symlink.sh
cd $(dirname $0)
for dotfile in .?*; do
    case $dotfile in
        *.elc)
            continue;;
        ..)
            continue;;
        .git)
            continue;;
        *)
            ln -Fis "$PWD/$dotfile" $HOME
            ;;
    esac
done
