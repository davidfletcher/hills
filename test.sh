#!/bin/bash

set -e -u

grep '^    hills' README.md | cut -d' ' -f6- | sort -u | \
    while read xxx; do
        echo dist/build/hills/hills -i $HOME/share/hills $xxx +RTS -K1K
        dist/build/hills/hills -i $HOME/share/hills $xxx +RTS -K1K
    done
