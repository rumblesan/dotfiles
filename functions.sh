#! /bin/bash

# Some useful functions

repos()
{
    if [ -z '$1' ]; then
        cd "${REPODIR}"
    else
        cd "${REPODIR}/$1"
    fi
}

function groot() {

  cd `git rev-parse --show-toplevel`

}

function home-mutt() {
  cd "$HOME"
  mutt -f "$HOME/.mail/Personal/INBOX"
}

function work-mutt() {
  cd "$HOME"
  mutt -f "$HOME/.mail/Pusher/INBOX"
}

