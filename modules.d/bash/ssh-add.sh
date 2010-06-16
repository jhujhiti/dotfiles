#!/bin/bash
### depends: module lib
### depends: quick_which ssh-add
### depends: [ -r ~/.ssh/id_rsa ]

ssh-add -L | grep `awk '{ print $2; };' ~/.ssh/id_rsa.pub` > /dev/null
if [ $? -eq 1 ]; then
    ssh-add ~/.ssh/id_rsa
fi
