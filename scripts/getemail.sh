#! /bin/bash

BREW_DIR=`brew --prefix`

$BREW_DIR/bin/offlineimap -u basic

$BREW_DIR/bin/notmuch new
