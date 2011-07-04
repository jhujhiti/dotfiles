#!/bin/bash
### depends: module lib
### depends: interactive
### depends: quick_which gpg-agent

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
