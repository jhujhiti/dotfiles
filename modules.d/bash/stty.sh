#!/bin/bash
### depends: module lib
### depends: interactive
### depends: quick_which stty

# disable terminal flow control. good riddance
stty stop ^@ 2>/dev/null
stty start ^@ 2>/dev/null
