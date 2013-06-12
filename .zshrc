# history
HISTFILE=~/.zsh_history
HISTSIZE=5000
SAVEHIST=5000

# enable extended globbing
setopt extendedglob
# warn me if a glob doesn't produce anything
setopt nomatch
# traps and options set inside a function will revert to their original
# setting after function return
setopt local_{traps,options}
# don't just execute bang history (eg, !!) -- show me the expanded command
setopt hist_verify
# append to the history file
setopt appendhistory
# and do it as it happens
setopt inc_append_history
# and share it with all of the other zshs
setopt share_history
# don't keep duplicate commands in history
setopt hist_ignore_all_dups
# ignore lines that begin with space
setopt hist_ignore_space
# and the history and fc commands
setopt hist_no_store
# stop beeping at me
unsetopt beep
# don't cd to a directory automatically if i forget the cd
unsetopt autocd
# don't interrupt me if a background job finishes while i'm typing
unsetopt notify

# emacs line editing mode
bindkey -e

# completion stuff
zstyle :compinstall filename '~/.zshrc'

autoload -Uz compinit
compinit

platform=$(uname -s)
linux() {
    if [[ "$platform" == "Linux" ]]; then
        return 0
    else
        return 1
    fi
}
freebsd() {
    if [[ "$platform" == "FreeBSD" ]]; then
        return 0
    else
        return 1
    fi
}
darwin() {
    if [[ "$platform" == "Darwin" ]]; then
        return 0
    else
        return 1
    fi
}

# which on different platforms behaves differently
safe_which() {
    local TMP
    TMP=`which $1 2>/dev/null`
    if [ $? -eq 0 ]; then
        # for Solaris:
        if [ -z "${TMP/*no $1*}" ]; then
            echo ""
            return 1
        fi
    fi
    # for OpenBSD:
    if [ -z "${TMP/*Command not found.}" ]; then
        echo ""
        return 1
    fi
    echo $TMP
    return 0
}

quick_which() {
    safe_which "$1" 1>/dev/null
    return $?
}

if linux; then
    alias ls="ls --color=yes -Fh"
    alias ll="ls --color=yes -Fhl"
elif freebsd || darwin; then
    alias ls="ls -FGh"
    alias ll="ls -FGhl"
else
    alias ls="ls -Fh"
    alias ll="ls -Fhl"
fi

quick_which vim && export EDITOR=vim

if $(quick_which gpg-agent); then
    # read the file first, so if gpg-agent is already running, it will
    # be found
    if [ -f "${HOME}/.gpg-agent-info" ]; then
        . "${HOME}/.gpg-agent-info"
        export GPG_AGENT_INFO
    fi

    # start if we're not running
    gpg-agent > /dev/null 2>&1 || \
        gpg-agent --daemon \
        --write-env-file "${HOME}/.gpg-agent-info" > /dev/null 2>&1

    # read the info file and export its variables
    if [ -f "${HOME}/.gpg-agent-info" ]; then
        . "${HOME}/.gpg-agent-info"
        export GPG_AGENT_INFO
        # uncomment these if we ever replace ssh-agent
        #export SSH_AUTH_SOCK
        #export SSH_AGENT_PID
    fi

    # debian scripts fail without setting this
    export GPG_TTY=`tty`
fi

if $(quick_which ssh-add && [[ -r ~/.ssh/id_rsa ]]); then
    ssh-add -L 2>/dev/null | grep `awk '{ print $2; };' ~/.ssh/id_rsa.pub` > /dev/null
    if [ $? -eq 1 ]; then
        ssh-add ~/.ssh/id_rsa 2>/dev/null
    fi
fi

# disable terminal flow control. good riddance
if $(quick_which stty); then
    stty stop ^@ 2>/dev/null
    stty start ^@ 2>/dev/null
fi

# prompt
PS1="[%D{%b %d %H:%M} %~]
%m%# "

# these two environment variables are handy for automated debian changelog
# editing and probably other things too
export NAME="Erick Turnquist"
export EMAIL="jhujhiti@adjectivism.org"
# also for debian, from the "New Maintainer's Guide"
export DEBEMAIL="$EMAIL"
export DEBFULLNAME="$NAME"
# also handy. i can't for the life of me remember this
export GPGKEYID="8C1BFCC5"

# pager for quagga vtysh
export VTYSH_PAGER=cat

# timezone
export TZ="America/New_York"

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
