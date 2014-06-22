#! /bin/bash

BIN_DIR="/opt/boxen/homebrew/bin"
DATE=`date +%F - %T`

echo "\n"
echo "$DATE: Retrieving Email"
$BIN_DIR/offlineimap -u basic

echo "\n"
echo "$DATE: Indexing Email"
$BIN_DIR/notmuch new
