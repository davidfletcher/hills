#!/bin/bash

set -e -u

grep '^    hills' README.md | cut -d' ' -f6- | sort -u | \
    while read xxx; do
        echo cabal run hills -- -i $HOME/share/hills $xxx +RTS -K1K
        cabal run hills -- -i $HOME/share/hills $xxx +RTS -K1K
    done
