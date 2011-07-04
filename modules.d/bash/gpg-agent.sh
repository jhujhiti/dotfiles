#!/bin/bash
### depends: module lib
### depends: interactive
### depends: quick_which gpg-agent

# start if we're not running
# TODO: --use-standard-socket doesn't work with remote home directories
gpg-agent > /dev/null 2>&1 ||
    gpg-agent --use-standard-socket --daemon > /dev/null 2>&1
