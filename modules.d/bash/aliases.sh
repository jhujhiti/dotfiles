#!/bin/bash
### depends: module platform
### depends: interactive

if linux || nexenta; then
    alias ls="ls --color=yes -AFh"
    alias ll="ls --color=yes -AFlh"
    # I like ll to show me the SELinux label
    if quick_which /usr/sbin/selinuxenabled; then
        if /usr/sbin/selinuxenabled; then
            alias ll="ls --color=yes -AFlhZ"
        fi
    fi
elif [ -x /sw/bin/ls ]; then
    alias ls="/sw/bin/ls --color=yes -AFh"
    alias ll="/sw/bin/ls --color=yes -AFlh"
elif freebsd || darwin; then
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
