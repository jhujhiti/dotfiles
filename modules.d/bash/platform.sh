#!/bin/bash
### depends: module lib

nexenta() {
    [ -n "$IS_NEXENTA" ] && debug "short-circuit is_nexenta()" && \
        return $IS_NEXENTA
    IS_NEXENTA=1
    [[ "$(uname -v)" =~ Nexenta* ]] && IS_NEXENTA=0 && return 0
    return 1
}

linux() {
    [ -n "$IS_LINUX" ] && return $IS_LINUX
    IS_LINUX=1
    [[ "$(uname -s)" == "Linux" ]] && IS_LINUX=0 && return 0
    return 1
}

sunos() {
    [ -n "$IS_SUNOS" ] && return $IS_SUNOS
    IS_SUNOS=1
    [[ "$(uname -s)" == "SunOS" ]] && IS_SUNOS=0 && return 0
    return 1
}

darwin() {
    [ -n "$IS_DARWIN" ] && return $IS_DARWIN
    IS_DARWIN=1
    [[ "$(uname -s)" == "Darwin" ]] && IS_DARWIN=0 && return 0
    return 1
}

netbsd() {
    [ -n "$IS_NETBSD" ] && return $IS_NETBSD
    IS_NETBSD=1
    [[ "$(uname -s)" == "NetBSD" ]] && IS_NETBSD=0 && return 0
    return 1
}

freebsd() {
    [ -n "$IS_FREEBSD" ] && return $IS_FREEBSD
    IS_FREEBSD=1
    [[ "$(uname -s)" == "FreeBSD" ]] && IS_FREEBSD=0 && return 0
    return 1
}

openbsd() {
    [ -n "$IS_OPENBSD" ] && return $IS_OPENBSD
    IS_OPENBSD=1
    [[ "$(uname -s)" == "OpenBSD" ]] && IS_OPENBSD=0 && return 0
    return 1
}
