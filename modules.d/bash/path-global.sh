#!/bin/bash
### depends: module lib

for p in "$HOME/bin" "/sbin" "/usr/sbin" "/usr/local/bin" "/usr/local/sbin" \
    "/opt/local/bin" "/opt/local/sbin" "/sw/bin" "/sw/sbin" "/usr/pkg/bin" \
    "/usr/pkg/sbin"
do
    [ -d "$p" ] && my_prepend_path "$p"
done
