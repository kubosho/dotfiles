#!/bin/sh

readonly CURRENT_DIR=$(cd $(dirname $0);pwd)
readonly IGNORE_FILES=("." ".." ".git" ".bin" "*.elc" "${0##*/}")

if [ $CURRENT_DIR != $PWD ]; then
  cd $CURRENT_DIR
fi

for dotfile in .?*; do
    is_ignored=0

    for ignore_file in ${IGNORE_FILES[@]}; do
      if [ $dotfile = $ignore_file ]; then
        is_ignored=1
        break
      fi
    done

    if [ $is_ignored -ne 1 ]; then
      ln -isv "$CURRENT_DIR/$dotfile" $HOME
    fi
done

