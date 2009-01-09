export UNAME_S=$(uname -s)
UNAME_M=$(uname -m)

set -o emacs
alias e='emacs'
alias v='vim'

export CVS_RSH="ssh"

########## library functions ##########

# add something to $PATH if it's not already there
my_prepend_path() {
    TMP=$(echo "$1" | sed -e 's/\//\\\//g')
    if [ -n "${PATH/*$TMP:*}" ]; then
        export PATH="$1:$PATH"
    fi
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

########## editor ########

TMP=`safe_which 'vim'`
if [ -n "$TMP" ]; then
    export EDITOR="$TMP"
else
    TMP=`safe_which 'emacs'`
    if [ -n "$TMP" ]; then
        export EDITOR="$TMP"
    fi
fi

########## noisy terminal? ##########

# set NOISY if we should do things like spit out fortunes
case "$TERM" in
    "screen" | "screen-bce" | "dumb" | "su")
    ;;
    * )
    NOISY=1
    ;;
esac

########## path stuff ##########

for p in "$HOME/bin" "/sbin" "/usr/sbin"
do
    my_prepend_path $p
done

if [ "$UNAME_S" == "SunOS" ]; then
    for p in "/usr/xpg4/bin" "/usr/xpg6/bin" "/usr/sfw/bin" "/usr/sfw/sbin" \
        "/opt/csw/bin" "/opt/csw/sbin" "/usr/local/bin" "/usr/local/sbin"
    do
        my_prepend_path $p
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
        for d in `find /cust/ -type d -name "$t"`
        do
            my_prepend_path "$d"
        done
    done
fi

#####################################
########## code goes here! ##########
#####################################

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

# do we have sbcl? make sure the SBCL_HOME variable is set appropriately
TMP=`safe_which 'sbcl'`
if [ -n "$TMP" ]; then
    if [ "$TMP" == "/cust/bin/sbcl" ]; then
        export SBCL_HOME="${TMP/bin\/sbcl}lib/sbcl"
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

## pretty prompt

if [ "$UNAME_S" == "Linux" ]; then
    case "$UNAME_M" in
        sparc* ) CPU_COUNT=$(sed -ne 's!^ncpus active[[:space:]]\+: \([[:digit:]]\+\).*!\1!p' /proc/cpuinfo);;
        x86_64 | i686) 
        CPU_COUNT=$(grep -c '^processor[[:space:]]*' /proc/cpuinfo)
        #if [ `grep -c '^flags.*ht' /proc/cpuinfo` -gt 0 ]; then
        ## looks like ht, but amd's multicore chips report as ht to fool
        ## poorly written apps
        #if [ `grep -c 'AuthenticAMD' /proc/cpuinfo` -lt 1 ]; then
        # if we're not AMD, kill half of the CPU_COUNT to account for ht
        #    CPU_COUNT=$((CPU_COUNT / 2))
        #fi
        #fi
        ;;
        * ) CPU_COUNT=1;; #guess for now...
    esac
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
        echo -e '\033[01;31m'
    elif [ $(expr $tmp '>=' $(expr $CPU_COUNT)) == "1" ] ; then
        echo -e '\033[01;33m'
    else
        echo -e '\033[01;32m'
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
    echo '$(uptime | sed "s/^.*load average[s]\{0,1\}: \([[:digit:]]\{1,\}\...\).*$/\1/")'
}

if [ "$UNAME_S" == "Linux" ]; then
    PS1L='\[$(ps1_load_colour $(cut -d " " -f 1 /proc/loadavg))\]$(cut -d " " -f 1 /proc/loadavg)'
else
    #ONEMIN='$(uptime | sed "s/^.*load average[s]\{0,1\}: \(.\...\).*$/\1/")'
    #PS1L="\[$(ps1_load_colour $ONEMIN)\]$ONEMIN"
    PS1L='\[$(ps1_load_colour '"$(onemin)"')\]'"$(onemin)"
fi

PS1P='\$'
PS1Z='\[\033[00m\]'

case "$TERM" in
    xterm* ) export PS1="\[\033[00m\]${PS1U}${PS1H} ${PS1D} ${PS1L} ${PS1P} ${PS1Z}\[\033]2;\u@\h \w\007\]";;
    screen* ) export PS1="\[\033[00m\]${PS1U}${PS1H} ${PS1D} ${PS1L} ${PS1P} ${PS1Z}\[\033]2;\u@\h \w\007\]";;
    rxvt* ) export PS1="\[\033[00m\]${PS1U}${PS1H} ${PS1D} ${PS1L} ${PS1P} ${PS1Z}\[\033]2;\u@\h \w\007\]";;
    * ) export PS1="${PS1U}${PS1H} ${PS1D} ${PS1L} ${PS1P} ${PS1Z}";; 
esac

export TERMBGC="dark"

if [ "$TERM" == "xterm" -a "$UNAME_S" == "SunOS" ]; then
    export TERM="dtterm"
fi
if [ "$TERM" == "xterm-color" -a "$UNAME_S" == "SunOS" ]; then
    export TERM="dtterm"
fi
if [ "$TERM" == "rxvt" -a "$UNAME_S" == "SunOS" ]; then
    export TERM="dtterm"
fi


if [ -n "$NOISY" ]; then
    TMP=`safe_which fortune`    
    if [ -n "$TMP" ]; then  
        $TMP
    fi
fi

umask 0022

screen_title() {
    echo -n -e "\033k${1}\033\\"
}

# if we have a termcap for rxvt-unicode, use it on certain boxes
# (i'll be using urxvt in that case anyway, and this will make vim colors
# work in screen)
if [ "$(hostname)" = "kali" ]; then
    safe_which toe 1>/dev/null && toe -a | grep 'rxvt-unicode' 1>/dev/null && export TERM=rxvt-unicode
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
