#!/bin/bash
### depends: module platform
### depends: interactive

if linux || nexenta; then
    alias ls="ls --color=yes -Fh"
    alias ll="ls --color=yes -Flh"
elif [ -x /sw/bin/ls ]; then
    alias ls="/sw/bin/ls --color=yes -Fh"
    alias ll="/sw/bin/ls --color=yes -Flh"
elif freebsd || darwin; then
    alias ls="ls -FGh"
    alias ll="ls -FGhl"
else
    alias ls='ls -Fh'
    alias ll='ls -Fhl'
fi

if sunos; then
    alias pfe="pfexec"
else
    alias pfe="sudo"
fi
