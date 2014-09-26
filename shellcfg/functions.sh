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

t () {

    # Start tmux in every new shell
    if [ -z "$TMUX" ]; then
        export TERM='xterm-256color'
        tmux
    else
        die "Not nesting sessions"
    fi

}


