#!/bin/bash

die()
{
    echo >&2 "$*"
    exit 1
}

#REPOENV=`git config --get repo.environment`
#
#if [ "$REPOENV" == "work" ]; then
#    egrep -q "^Issue #[0-9]+ " "$1"
#    CHECK=$?
#
#    if [ $CHECK != 0 ]; then
#        die "No Github issue number in commit"
#    fi
#fi

