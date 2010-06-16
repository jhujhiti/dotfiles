#!/bin/bash

ROOT="$HOME/dotfiles/"

[ -r "$ROOT/bash_loader" ] && source "$ROOT/bash_loader"

[ -r ~/.shellconfig ] && source ~/.shellconfig
