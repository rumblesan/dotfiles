#! /bin/bash

BIN_DIR="/opt/boxen/homebrew/bin"
DATE=`date +%F-%T`

echo ""
echo "$DATE: Retrieving Email"
$BIN_DIR/offlineimap -u basic

echo ""
echo "$DATE: Indexing Email"
$BIN_DIR/notmuch new

