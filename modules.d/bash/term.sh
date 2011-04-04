#!/bin/bash
### depends: module prompt

case "$TERM" in
    xterm* | screen* | rxvt* )
    PS1="\[\033[00m\]${PS1U}${PS1H} ${PS1D} ${PS1L} ${PS1P} ${PS1Z}\[\033]0;\u@\h \w\007\]"
    ;;

    * )
    PS1="${PS1U}${PS1H} ${PS1D} ${PS1L} ${PS1P} ${PS1Z}"
    ;; 
esac

screen_title() {
    echo -n -e "\033k${1}\033\\"
}

if [ sunos -a ! nexenta ]; then
    case "$TERM" in
        xterm*|rxvt*)
        export TERM=dtterm
    esac
fi
