#!/bin/bash
### depends: module lib
### depends: quick_which dircolors
### depends: disabled

if [ "$UNAME_S" == "Darwin" -a -r ~/dotfiles/dircolors_darwin ]; then
    eval `$TMP ~/dotfiles/dircolors_darwin`
elif [ -r ~/dotfiles/dircolors ]; then
    eval `$TMP ~/dotfiles/dircolors`
fi
