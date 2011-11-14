#!/bin/bash
### depends: module lib
### depends: interactive
### depends: quick_which ssh-add
### depends: [ -r ~/.ssh/id_rsa ]

ssh-add -L 2>/dev/null | grep `awk '{ print $2; };' ~/.ssh/id_rsa.pub` > /dev/null
if [ $? -eq 1 ]; then
    ssh-add ~/.ssh/id_rsa 2>/dev/null
fi
