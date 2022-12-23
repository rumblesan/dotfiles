#!/usr/bin/env bash

# exit when script tries to use an undeclared variable
set -o nounset
# exit on error
set -o errexit
# exit if anything in a pipe fails
set -o pipefail

[[ "${DEBUG:-}" == 'true' ]] && set -o xtrace

readonly SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

die() {
    echo "$*"
    exit 1
}

script()
{
    local script="${SCRIPT_DIR}/scripts/$1"
    if [[ -f $script ]]; then
        $script
    else
        unknown $1
    fi
}

unknown()
{
    echo "Don't recognise '$1'"
}

runaction()
{
    action=$( printf "%s\n" "$1" | tr 'A-Z' 'a-z' )

    case "$action" in
    "sleep" )
        pmset sleepnow
        ;;
    "lock" )
        pmset displaysleepnow
        ;;
    "exit" )
        exit 0
        ;;
    * )
        script "$action"
        ;;
    esac

}


main () {
    while true; do
        read -p ">>> " line
        runaction "$line"
    done
}

main "$@"
