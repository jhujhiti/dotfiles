export UNAME_S=$(uname -s)
UNAME_M=$(uname -m)

set -o emacs

export CVS_RSH="ssh"
export PAGER="less"

# these two environment variables are handy for automated debian changelog
# editing and probably other things too
export NAME="Erick Turnquist"
export EMAIL="jhujhiti@adjectivism.org"
# also handy. i can't for the life of me remember these key ids
export GPGKEYID="77E7AD4B"
export GPGKEYID_PKG="7ACC1CBC"

########## library functions ##########

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

pidcmd() {
    local ZPID="$1"
    local CMD=$(ps -eo pid,comm | egrep "^[[:space:]]*$ZPID[[:space:]]" | \
        awk '{print $2;}')
    echo "$CMD" | grep '/' 1>/dev/null
    if [ $? -eq 0 ]; then
        echo $(echo "$CMD" | awk -F/ '{print $NF;}')
    else
        echo "$CMD"
    fi
    return 0
}

########## path stuff ##########

for p in "$HOME/bin" "/sbin" "/usr/sbin"
do
    my_prepend_path $p
done

## find paths for ruby gems
gem_path() {
    GEMPATH=$(gem environment gempath)
    for p in ${GEMPATH//:/$'\n'}
    do
        [ -d "$p/bin" ] && my_prepend_path "$p/bin"
    done
}
safe_which gem 1>/dev/null && gem_path

if [ "$UNAME_S" == "SunOS" ]; then
    for p in "/usr/xpg4/bin" "/usr/xpg6/bin" "/usr/sfw/bin" "/usr/sfw/sbin" \
        "/opt/csw/bin" "/opt/csw/sbin" "/usr/local/bin" "/usr/local/sbin"
    do
        [ -d "$p" ] && my_prepend_path $p
    done
    if [ -d /usr/ccs/bin ]; then
        my_prepend_path "/usr/ccs/bin"
        if [ -d /usr/ccs/bin/sparcv9 ]; then
            my_prepend_path "/usr/ccs/bin/sparcv9"
        fi
    fi

    export MANPATH="/usr/local/man:/opt/csw/man:/usr/sfw/man:/usr/man"
    if [ -d /cust/man ]; then
        export MANPATH="/cust/man:$MANPATH"
    fi
    if [ -d /cust/share/man ]; then
        export MANPATH="/cust/share/man:$MANPATH"
    fi
fi

if [ "$UNAME_S" == "Darwin" ]; then
    #export TERM="xterm" # why?
    if [ -d /opt/local ]; then
        my_prepend_path "/opt/local/bin"
        my_prepend_path "/opt/local/sbin"
    fi
    test -r /sw/bin/init.sh && source /sw/bin/init.sh
fi

if [ "$UNAME_S" == "NetBSD" ]; then
    my_prepend_path "/usr/pkg/bin"
    my_prepend_path "/usr/pkg/sbin"
fi

if [ -d /cust ]; then
    for t in "bin" "sbin"
    do
        [ -d "/cust/$t" ] && my_prepend_path "/cust/$t"
    done
fi

########## editor ########

TMP=`safe_which 'vim'`
if [ -n "$TMP" ]; then
    export EDITOR="$TMP"
fi

#####################################
########## code goes here! ##########
#####################################

# set up bash completion if we have it
[ -f /etc/bash_completion ] && . /etc/bash_completion

# disable terminal flow control. good riddance
safe_which stty 1>/dev/null && stty stop ^@ 2>/dev/null ; \
    stty start ^@ 2>/dev/null

# add ssh-agent keys from ~/.ssh/id_rsa if we have it
TMP=`safe_which 'ssh-add'`
if [ -n "$TMP" ]; then
    if [ -r ~/.ssh/id_rsa ]; then
        $TMP -L | grep `awk '{ print $2; };' ~/.ssh/id_rsa.pub` > /dev/null
        if [ $? -eq 1 ]; then
            $TMP ~/.ssh/id_rsa
        fi
    fi
fi

# color for ls. from dircolors on debian
TMP=`safe_which 'dircolors'`
if [ -n "$TMP" ]; then
    if [ "$UNAME_S" == "Darwin" -a -r ~/dotfiles/dircolors_darwin ]; then
        eval `$TMP ~/dotfiles/dircolors_darwin`
    elif [ -r ~/dotfiles/dircolors ]; then
        eval `$TMP ~/dotfiles/dircolors`
    fi
fi

## handy aliases
if [ "$UNAME_S" == "Linux" ]; then
    alias ls="ls --color=yes -AFh"
    alias ll="ls --color=yes -AFlh"
elif [ -x /sw/bin/ls ]; then
    alias ls="/sw/bin/ls --color=yes -AFh"
    alias ll="/sw/bin/ls --color=yes -AFlh"
elif [ "$UNAME_S" == "FreeBSD" -o "$UNAME_S" == "Darwin" ]; then
    alias ls="ls -AFGh"
    alias ll="ls -AFGhl"
else
    alias ls='ls -AFh'
    alias ll='ls -AFhl'
fi

alias grab="sudo chown ${USER} --recursive"
alias e='emacs'
alias v='vim'
if [ "$UNAME_S" == "SunOS" ]; then
    alias pfe="pfexec"
else
    alias pfe="sudo"
fi

## pretty prompt

if [ "$UNAME_S" == "Linux" ]; then
    CPU_COUNT=$(getconf _NPROCESSORS_ONLN)
elif [ "$UNAME_S" == "Darwin" -o "$UNAME_S" == "FreeBSD" ]; then
    CPU_COUNT=$(sysctl hw.ncpu | sed 's!^hw\.ncpu\: \([[:digit:]]\{1,\}\)!\1!')
elif [ "$UNAME_S" == "NetBSD" ]; then
    CPU_COUNT=$(sysctl hw.ncpu | sed 's!^hw\.ncpu = \([[:digit:]]\{1,\}\)!\1!')
elif [ "$UNAME_S" == "OpenBSD" ]; then
    CPU_COUNT=$(sysctl hw.ncpu | awk -F '=' '{ print $2; };')
elif [ "$UNAME_S" == "SunOS" ]; then
    #TODO: fix for multicore systems. see: psrinfo -pv
    CPU_COUNT=$(/usr/sbin/psrinfo | grep -c 'on-line')
fi

ps1_load_colour() {
    tmp=${1%%.*}
    if [ $(expr $tmp '>=' $(expr $CPU_COUNT '*' 2)) == "1" ] ; then
        echo -ne '\033[01;31m'
    elif [ $(expr $tmp '>=' $(expr $CPU_COUNT)) == "1" ] ; then
        echo -ne '\033[01;33m'
    else
        echo -ne '\033[01;32m'
    fi
}

if [ "$UNAME_S" == "SunOS" ]; then
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
    if [ "$UNAME_S" == "Linux" ]; then
        echo '$(cut -d " " -f 1 /proc/loadavg)'
    else
        echo '$(uptime | sed "s/^.*load average[s]\{0,1\}: \([[:digit:]]\{1,\}\...\).*$/\1/")'
    fi
}

PS1L='\[$(ps1_load_colour '"$(onemin)"')\]'"$(onemin)"

PS1P='\$'
PS1Z='\[\033[00m\]'

case "$TERM" in
    xterm* ) export PS1="\[\033[00m\]${PS1U}${PS1H} ${PS1D} ${PS1L} ${PS1P} ${PS1Z}\[\033]0;\u@\h \w\007\]";;
    screen* ) export PS1="\[\033[00m\]${PS1U}${PS1H} ${PS1D} ${PS1L} ${PS1P} ${PS1Z}\[\033]0;\u@\h \w\007\]";;
    rxvt* ) export PS1="\[\033[00m\]${PS1U}${PS1H} ${PS1D} ${PS1L} ${PS1P} ${PS1Z}\[\033]0;\u@\h \w\007\]";;
    * ) export PS1="${PS1U}${PS1H} ${PS1D} ${PS1L} ${PS1P} ${PS1Z}";; 
esac

export TERMBGC="dark"

umask 0022

screen_title() {
    echo -n -e "\033k${1}\033\\"
}

if [ "$UNAME_S" == "SunOS" ]; then
    case "$TERM" in
        xterm*|rxvt*)
        export TERM=dtterm
    esac
fi

# wrapper around pushd .. ; popd
pdir() {
    [ -z "$*" ] && echo "Usage: pdir command [..]" && return 1
    cmd=$1
    old="$OLDPWD"
    shift
    pushd ..
    $cmd $@
    ret=$?
    popd
    export OLDPWD="$old"
    return $ret
}

# shellconfig
if [ -r ~/.shellconfig ]; then
    source ~/.shellconfig
fi
