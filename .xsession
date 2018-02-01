#!/bin/sh

# which on different platforms behaves differently
safe_which() {
    local TMP
    TMP=`which $1 2>/dev/null`
    ret=$?
    if [ $ret -eq 0 ]; then
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
    return $ret
}

quick_which() {
    safe_which "$1" 1>/dev/null
    return $?
}

# this doesn't work when .xsession is sourced because $0 is "wrong".
# we'll hardcode $HOME/dotfiles for now and hope it always works. it probably will.
#if [ $(uname -s) == 'Linux' ]; then
#    dotfiles=$(dirname $(readlink -f $0))
#else
#    dotfiles=$(dirname $(realpath $0))
#fi
dotfiles="${HOME}/dotfiles"

# load the custom xkbsymbols into the second group
if $(quick_which setxkbmap) && $(quick_which xkbcomp); then
    setxkbmap -print | sed -e '/xkb_symbols/s/"[[:space:]]/+local:2&/' | xkbcomp -w0 -I"$dotfiles/xkb" - $DISPLAY
fi
