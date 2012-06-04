#!/bin/bash
### depends: module prompt

case "$TERM" in
    xterm* | screen* | rxvt* )
        PS1="$(eval echo "[${PS1DATE} ${PS1D}${PS1DE}]${PS1SELINUX}\\\n${PS1U}${PS1H} ${PS1L} ${PS1P} ${PS1Z}")\[\033]0;\h \w\007\]"
        ;;

    * )
        PS1="$(eval echo "[${PS1DATE} ${PS1D}${PS1DE}]${PS1SELINUX}\\\n${PS1U}${PS1H} ${PS1L} ${PS1P} ${PS1Z}")"
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
