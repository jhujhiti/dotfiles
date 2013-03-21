#!/bin/bash
### depends: interactive
### depends: quick_which at

atin() {
    when=$(date --date "now + $1" '+%H%M %b %d %Y')
    at "$when"
}
