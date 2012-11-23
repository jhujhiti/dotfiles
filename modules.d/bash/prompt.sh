#!/bin/bash
### depends: module platform
### depends: module git
### depends: interactive

PS1U="\\\u"

PS1H="@\\\h"
PS1D="\\\w"

gps1() {
    echo -n "\$(__git_ps1 ' {%s}')"
}

PS1DE="\$(gps1)"

PS1SELINUX=""
if quick_which /usr/sbin/selinuxenabled; then
    if /usr/sbin/selinuxenabled; then
        PS1SELINUX="\\\n\<$(id -Z)\>"
    fi
fi

onemin() {
    if linux; then
        echo '$(cut -d " " -f 1 /proc/loadavg)'
    else
        echo '$(uptime | sed "s/^.*load average[s]\{0,1\}: \([[:digit:]]\{1,\}\...\).*$/\1/")'
    fi
}

PS1L="\$(onemin)\${ANSI_RESET}"

if sunos; then
    if [ $(/usr/xpg4/bin/id -un) == "root" ]; then
        PS1P="\${ANSI_BRIGHT}\${ANSI_RED}#\${ANSI_RESET}"
    else
        PS1P="$"
    fi
else
    if [ `/usr/bin/whoami` = 'root' ]; then
        PS1P="\${ANSI_BRIGHT}\${ANSI_RED}#\${ANSI_RESET}"
    else
        PS1P="$"
    fi
fi

PS1DATE="\\\D{%b %d %H:%M}"

PS1Z="\${ANSI_RESET}"
