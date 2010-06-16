#!/bin/bash
### depends: module platform

if linux -o nexenta; then
    alias ls="ls --color=yes -AFh"
    alias ll="ls --color=yes -AFlh"
elif [ -x /sw/bin/ls ]; then
    alias ls="/sw/bin/ls --color=yes -AFh"
    alias ll="/sw/bin/ls --color=yes -AFlh"
elif freebsd -o darwin; then
    alias ls="ls -AFGh"
    alias ll="ls -AFGhl"
else
    alias ls='ls -AFh'
    alias ll='ls -AFhl'
fi

if sunos; then
    alias pfe="pfexec"
else
    alias pfe="sudo"
fi
