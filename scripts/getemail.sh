#! /bin/bash

BIN_DIR="/opt/boxen/homebrew/bin"
DATE=`date +%F-%T`

echo ""
echo "$DATE: Retrieving Email"
$BIN_DIR/offlineimap -u basic

echo ""
echo "$DATE: Indexing Email"
$BIN_DIR/notmuch new

NEWMAIL=$("$BIN_DIR/notmuch" count tag:unread)
if [ "$NEWMAIL" != "0" ]; then
    echo "New mail received"
    "$BIN_DIR/terminal-notifier" -title "New mail" -message "You have new Emails"
fi

