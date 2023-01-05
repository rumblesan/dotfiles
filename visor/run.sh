#!/usr/bin/env bash

# exit when script tries to use an undeclared variable
set -o nounset
# exit on error
set -o errexit
# exit if anything in a pipe fails
set -o pipefail

[[ "${DEBUG:-}" == 'true' ]] && set -o xtrace

PATH=/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin
if [ -d /opt/homebrew/bin/ ];then
    export PATH="/opt/homebrew/bin:$PATH"
fi
eval "$(brew shellenv)"

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

app()
{
    local appfolders=(
        /Applications
        /Applications/Utilities
        /System/Applications
        /System/Applications/Utilities
        /System/Library/PreferencePanes
    )
    local appname="$(find ${appfolders[*]} -name '*.app' -o -name '*.prefPane' -maxdepth 1 | fzf)"
    if [[ ! -z "$appname" ]]; then
        open "$appname"
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
    "app" )
        app
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

    local scripts="ls ${SCRIPT_DIR}/scripts/"
    local commands=(
        "sleep"
        "app"
        "lock"
        "exit"
    )

    while true; do
        local line=`(${scripts[@]}; printf "%s\n" ${commands[@]}) | fzf`
        if [[ ! -z "$line" ]]; then
            runaction "$line"
        fi
    done
}

main "$@"
