#!/bin/bash

# I have no better name for this. It's a bunch of miscellaneous, simple crap

set -o emacs

export CVS_RSH="ssh"

# these two environment variables are handy for automated debian changelog
# editing and probably other things too
export NAME="Erick Turnquist"
export EMAIL="jhujhiti@adjectivism.org"
# also for debian, from the "New Maintainer's Guide"
export DEBEMAIL="$EMAIL"
export DEBFULLNAME="$NAME"
# also handy. i can't for the life of me remember this
export GPGKEYID="77E7AD4B"

# pager for quagga vtysh
export VTYSH_PAGER=cat

umask 0077

ANSI_RESET='\[\033[0m\]'
ANSI_BRIGHT='\[\033[1m\]'
ANSI_DIM='\[\033[2m\]'
ANSI_UNDERSCORE='\[\033[4m\]'
ANSI_BLINK='\[\033[5m\]'
ANSI_REVERSE='\[\033[7m\]'
ANSI_HIDDEN='\[\033[8m\]'
ANSI_BLACK='\[\033[30m\]'
ANSI_RED='\[\033[31m\]'
ANSI_GREEN='\[\033[32m\]'
ANSI_YELLOW='\[\033[33m\]'
ANSI_BLUE='\[\033[34m\]'
ANSI_MAGENTA='\[\033[35m\]'
ANSI_CYAN='\[\033[36m\]'
ANSI_WHITE='\[\033[37m\]'
ANSI_BG_BLACK='\[\033[40m\]'
ANSI_BG_RED='\[\033[41m\]'
ANSI_BG_GREEN='\[\033[42m\]'
ANSI_BG_YELLOW='\[\033[43m\]'
ANSI_BG_BLUE='\[\033[44m\]'
ANSI_BG_MAGENTA='\[\033[45m\]'
ANSI_BG_CYAN='\[\033[46m\]'
ANSI_BG_WHITE='\[\033[47m\]'
