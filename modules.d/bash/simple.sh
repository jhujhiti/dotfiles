#!/bin/bash

# I have no better name for this. It's a bunch of miscellaneous, simple crap

set -o emacs

export CVS_RSH="ssh"

# these two environment variables are handy for automated debian changelog
# editing and probably other things too
export NAME="Erick Turnquist"
export EMAIL="jhujhiti@adjectivism.org"
# also handy. i can't for the life of me remember these key ids
export GPGKEYID="77E7AD4B"
export GPGKEYID_PKG="7ACC1CBC"

umask 0022
