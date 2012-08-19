#!/bin/bash
### depends: module lib
### depends: interactive
### depends: quick_which gpg-agent

# read the file first, so if gpg-agent is already running, it will be found
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
