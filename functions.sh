#! /bin/bash

# Some useful functions

repos()
{
    if [ -z '$1' ]; then
        cd "${HOME}/repositories"
    else
        cd "${HOME}/repositories/$1"
    fi
}

