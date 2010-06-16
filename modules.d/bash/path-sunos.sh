#!/bin/bash
### depends: module platform
### depends: sunos
### depends: ! nexenta

for p in "/usr/xpg4/bin" "/usr/xpg6/bin" "/usr/sfw/bin" \
    "/usr/sfw/sbin" "/opt/csw/bin" "/opt/csw/sbin"
    do
        [ -d "$p" ] && my_prepend_path $p
    done
if [ -d /usr/ccs/bin ]; then
    my_prepend_path "/usr/ccs/bin"
    if [ -d /usr/ccs/bin/sparcv9 ]; then
        my_prepend_path "/usr/ccs/bin/sparcv9"
    fi
fi
