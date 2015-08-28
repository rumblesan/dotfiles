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

dmuse()
{
    if [ -z "$1" ]; then
        echo "must give docker-machine vm name"
    else
        eval $(docker-machine env "$1")
    fi
}

