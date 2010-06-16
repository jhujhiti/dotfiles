#!/bin/bash
### depends: module platform

if linux ; then
    CPU_COUNT=$(getconf _NPROCESSORS_ONLN)
elif freebsd -o darwin ; then
    CPU_COUNT=$(sysctl hw.ncpu | sed 's!^hw\.ncpu\: \([[:digit:]]\{1,\}\)!\1!')
elif netbsd ; then
    CPU_COUNT=$(sysctl hw.ncpu | sed 's!^hw\.ncpu = \([[:digit:]]\{1,\}\)!\1!')
elif openbsd ; then
    CPU_COUNT=$(sysctl hw.ncpu | awk -F '=' '{ print $2; };')
elif sunos ; then
    CPU_COUNT=$(/usr/sbin/psrinfo | grep -c 'on-line')
fi

ps1_load_color() {
    tmp=${1%%.*}
    if [ $(expr $tmp '>=' $(expr $CPU_COUNT '*' 2)) == "1" ] ; then
        echo -ne '\033[01;31m'
    elif [ $(expr $tmp '>=' $(expr $CPU_COUNT)) == "1" ] ; then
        echo -ne '\033[01;33m'
    else
        echo -ne '\033[01;32m'
    fi
}

if sunos ; then
    if [ $(/usr/xpg4/bin/id -un) == "root" ]; then
        PS1U='\[\033[01;31m\]\u'
    else
        PS1U='\[\033[01;32m\]\u'
    fi
else
    if [ `/usr/bin/whoami` = 'root' ]; then
        PS1U='\[\033[01;31m\]\u'
    else
        PS1U='\[\033[01;32m\]\u'
    fi
fi

PS1H="\[\033[01;34m\]@\h"
PS1D='\[\033[01;34m\]\w'

onemin() {
    if linux ; then
        echo '$(cut -d " " -f 1 /proc/loadavg)'
    else
        echo '$(uptime | sed "s/^.*load average[s]\{0,1\}: \([[:digit:]]\{1,\}\...\).*$/\1/")'
    fi
}

PS1L='\[$(ps1_load_color '"$(onemin)"')\]'"$(onemin)"

PS1P='\$'
PS1Z='\[\033[00m\]'

case "$TERM" in
    xterm* ) export PS1="\[\033[00m\]${PS1U}${PS1H} ${PS1D} ${PS1L} ${PS1P} ${PS1Z}\[\033]0;\u@\h \w\007\]";;
    screen* ) export PS1="\[\033[00m\]${PS1U}${PS1H} ${PS1D} ${PS1L} ${PS1P} ${PS1Z}\[\033]0;\u@\h \w\007\]";;
    rxvt* ) export PS1="\[\033[00m\]${PS1U}${PS1H} ${PS1D} ${PS1L} ${PS1P} ${PS1Z}\[\033]0;\u@\h \w\007\]";;
    * ) export PS1="${PS1U}${PS1H} ${PS1D} ${PS1L} ${PS1P} ${PS1Z}";; 
esac

export TERMBGC="dark"

screen_title() {
    echo -n -e "\033k${1}\033\\"
}

if sunos -a ! nexenta ; then
    case "$TERM" in
        xterm*|rxvt*)
        export TERM=dtterm
    esac
fi
