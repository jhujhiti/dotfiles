#!/bin/bash
### depends: module platform
### depends: module git
### depends: interactive

if linux; then
    CPU_COUNT=$(getconf _NPROCESSORS_ONLN)
elif freebsd || darwin; then
    CPU_COUNT=$(sysctl hw.ncpu | sed 's!^hw\.ncpu\: \([[:digit:]]\{1,\}\)!\1!')
elif netbsd; then
    CPU_COUNT=$(sysctl hw.ncpu | sed 's!^hw\.ncpu = \([[:digit:]]\{1,\}\)!\1!')
elif openbsd; then
    CPU_COUNT=$(sysctl hw.ncpu | awk -F '=' '{ print $2; };')
elif sunos; then
    CPU_COUNT=$(/usr/sbin/psrinfo | grep -c 'on-line')
fi

ps1_load_color() {
    tmp=${1%%.*}
    if [ $(expr $tmp '>=' $(expr $CPU_COUNT '*' 2)) == "1" ] ; then
        echo -ne "${ANSI_BRIGHT}${ANSI_RED}"
    elif [ $(expr $tmp '>=' $(expr $CPU_COUNT)) == "1" ] ; then
        echo -ne "${ANSI_BRIGHT}${ANSI_YELLOW}"
    else
        echo -ne "${ANSI_RESET}"
    fi
}

PS1U="\\\u"

PS1H="@\\\h"
PS1D="\\\w"

gps1() {
    echo -n "\$(__git_ps1 ' {%s}')"
}

PS1DE="\$(gps1)"

onemin() {
    if linux; then
        echo '$(cut -d " " -f 1 /proc/loadavg)'
    else
        echo '$(uptime | sed "s/^.*load average[s]\{0,1\}: \([[:digit:]]\{1,\}\...\).*$/\1/")'
    fi
}

PS1L="\$(ps1_load_color "$(onemin)")\$(onemin)\${ANSI_RESET}"

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
