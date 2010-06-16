#!/bin/bash
### depends: module lib
### depends: quick_which gem

GEMPATH=$(gem environment gempath)
for p in ${GEMPATH//:/$'\n'}
do
    [ -d "$p/bin" ] && my_prepend_path "$p/bin"
done
