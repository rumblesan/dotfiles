#! /bin/bash

INSTALL_SH=$(basename "$0")
oneline_usage="$INSTALL_SH [-hf] command"

usage()
{
    cat <<-EndUsage
		Usage: $oneline_usage
		Use '$INSTALL_SH -h' for more information
	EndUsage
    exit 1
}

helpinfo()
{
cat <<-EndHelp
Usage: $oneline_usage

Commands:
    setup
        Create all the symlinks necessary for the dotfiles
    cleanup
        Delete all the dotfiles symlinks
    update
        Update all the git submodules
    firsttime
        Run the first time setup
    sync
        Sync this repo with the remote

Flags:
    -h
        Show usage info
    -f
        Delete files dithout asking user

EndHelp
exit 0
}

die()
{
    echo "$*"
    exit 1
}

setup()
{
    for FILE in *.dotfile; do
        FILE_NAME="$DOTFILE_DIR/$FILE"
        LINK_NAME=~/`echo ".$FILE" | sed "s/\.dotfile//"`
        linkfile "$FILE_NAME" "$LINK_NAME"
    done

    #ssh config
    FILE_NAME="$DOTFILE_DIR/sshconfig.symlink"
    LINK_NAME=~/.ssh/config
    linkfile "$FILE_NAME" "$LINK_NAME"

    FILE_NAME="$DOTFILE_DIR/cabalconfig.symlink"
    LINK_NAME=~/.cabal/config
    linkfile "$FILE_NAME" "$LINK_NAME"

    if [ ! -d "$MY_LOG_DIR" ]; then
        echo "Creating log dir"
        mkdir -p "$MY_LOG_DIR"
    fi
}

linkfile()
{
    FILE_NAME="$1"
    LINK_NAME="$2"
    echo "Creating $LINK_NAME"
    if [ -e "$LINK_NAME" ]; then
        echo "$LINK_NAME already exists!"
        if [ "$FORCE_DELETE" == "y" ]; then
            echo "Deleting"
            rm "$LINK_NAME"
            ln -s "$FILE_NAME" "$LINK_NAME"
        else
            RESPONSE="n"
            echo "Delete? [y|n]"
            read RESPONSE
            if [ ! -z "$RESPONSE" ]; then
                RESPONSE='n'
            fi
            if [ "$RESPONSE" == "y" ]; then
                echo "Deleting"
                rm "$LINK_NAME"
                ln -s "$FILE_NAME" "$LINK_NAME"
            else
                echo "Skipping"
            fi
        fi
    else
        ln -s "$FILE_NAME" "$LINK_NAME"
    fi
}


cleanup()
{
    for FILE in *.dotfile; do
        LINK_NAME=~/`echo ".$FILE" | sed "s/\.dotfile//"`
        deletefile "$LINK_NAME"
    done

    #ssh config
    LINK_NAME=~/.ssh/config
    deletefile "$LINK_NAME"
}

deletefile()
{
    LINK_NAME="$1"
    echo "$LINK_NAME"
    if [ "$FORCE_DELETE" == "y" ]; then
            echo "Deleting"
            rm "$LINK_NAME"
    else
        RESPONSE="n"
        echo "Delete? [y|n]"
        read RESPONSE
        if [ -z "$RESPONSE" ]; then
            RESPONSE='n'
        fi
        if [ "$RESPONSE" == "y" ]; then
            echo "Deleting"
            rm "$LINK_NAME"
        else
            echo "Skipping"
        fi
    fi
}

update()
{
    git submodule foreach git checkout master
    git submodule foreach git pull --rebase origin master
}

# Do all the misc setup on a new Mac
firsttime()
{
    # Ask for root password upfront
    sudo -v
    sudo cp "$DOTFILE_DIR/misc/fonts/*.ttf" /Library/Fonts/

    sudo "$DOTFILE_DIR/misc/osx.sh"
}

# Sync with remote repo
sync()
{
    git pull --rebase
    git submodule init
    git submodule update
    git push
}

runaction()
{
    if [ "$DOTFILE_DIR" == "$PWD" ]; then

        action=$( printf "%s\n" "$1" | tr 'A-Z' 'a-z' )

        case "$action" in
        "setup" )
            setup
            ;;
        "cleanup" )
            cleanup
            ;;
        "update" )
            update
            ;;
        "firsttime" )
            firsttime
            ;;
        "sync" )
            sync
            ;;
        * )
            usage
            ;;
        esac

    else
        die "This script needs to be run from the dotfiles directory:  $DOTFILE_DIR"
    fi
}

DOTFILE_DIR=~/.dotfiles
MY_LOG_DIR=~/.mylogs
PWD="`pwd`"

FORCE_DELETE="n"

while getopts "hf" opt "$@"; do
    case "$opt" in
        h)
            helpinfo
            ;;
        f)
            FORCE_DELETE="y"
            echo "Forcing Delete"
            ;;
    esac
done

runaction "${@:$OPTIND}"

