#! /bin/bash

# If there's a tty then we can use tputs
# If there's not then we don't define them
# TODO I probablly should define them using escape codes
if [[ $- =~ i ]]; then
    export BLACK=$(tput setaf 0)
    export RED=$(tput setaf 1)
    export GREEN=$(tput setaf 2)
    export YELLOW=$(tput setaf 3)
    export LIME_YELLOW=$(tput setaf 190)
    export POWDER_BLUE=$(tput setaf 153)
    export BLUE=$(tput setaf 4)
    export MAGENTA=$(tput setaf 5)
    export CYAN=$(tput setaf 6)
    export WHITE=$(tput setaf 7)
    export BRIGHT=$(tput bold)
    export NORMAL=$(tput sgr0)
    export BLINK=$(tput blink)
    export REVERSE=$(tput smso)
    export UNDERLINE=$(tput smul)
fi

