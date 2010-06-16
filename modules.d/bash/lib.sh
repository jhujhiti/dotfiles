#!/bin/bash

# debugging
debug() {
    [ -n "$SHELLDBG" ] && echo $1
}

# add something to $PATH if it's not already there
my_prepend_path() {
    local TMP=$(echo "$1" | sed -e 's/\//\\\//g')
    [ -n "${PATH/*$TMP:*}" ] && export PATH="$1:$PATH"
}

# which on different platforms behaves differently
safe_which() {
    local TMP=`which $1 2>/dev/null`
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

disabled() {
    return 1
}
