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

function n() { 
  if [ $# -eq 0 ]; then
    notes.sh ls
  else
    notes.sh $*
  fi
}

function c8() { 
  if [ $# -eq 0 ]; then
    com8.sh ls
  else
    com8.sh $*
  fi
}

function groot() {

  cd `git rev-parse --show-toplevel`

}

