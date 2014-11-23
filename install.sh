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
        Delete any dead dotfiles symlinks
    update
        Update all the git submodules
    sync
        Sync this repo with the remote
    firsttime
        Run the first time setup

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
    local filename=""
    local linkname=""
    local file=""

    local dotfiles=`find . -name "*.dotfile" -not -path "./.git/*" | sed "s|^\./||"`
    local symlinks=`find . -name "*.symlink" -not -path "./.git/*" | sed "s|^\./||"`

    for file in $dotfiles; do
        filename="$DOTFILE_DIR/$file"
        linkname=~/`echo ".$file" | sed "s/\.dotfile//"`
        linkfile "$filename" "$linkname"
    done

    for file in $symlinks; do
        filename="$DOTFILE_DIR/$file"
        linkname=~/`echo "$file" | sed "s/\.symlink//"`
        linkfile "$filename" "$linkname"
    done
}

linkfile()
{
    local response=""
    local filename="$1"
    local linkname="$2"
    echo "Creating $linkname"
    if [ -e "$linkname" ]; then
        echo "$linkname already exists!"
        if [ "$FORCE_DELETE" == "y" ]; then
            echo "Deleting"
            rm "$linkname"
            ln -s "$filename" "$linkname"
        else
            response="n"
            echo "Delete? [y|n]"
            read response
            if [ ! -z "$response" ]; then
                response='n'
            fi
            if [ "$response" == "y" ]; then
                echo "Deleting"
                rm "$linkname"
                ln -s "$filename" "$linkname"
            else
                echo "Skipping"
            fi
        fi
    else
        ln -s "$filename" "$linkname"
    fi
}


# Remove dead symlinks in home directory
cleanup()
{
    local link=""
    local symlinks=`find ~/ -maxdepth 1 -type l`
    for link in $symlinks; do
        if [ ! -e "$link" ]; then
            deletefile "$link"
        fi
    done
}

deletefile()
{
    local response=""
    local linkname="$1"
    echo "$linkname"
    if [ "$FORCE_DELETE" == "y" ]; then
        echo "Deleting"
        rm "$linkname"
    else
        response="n"
        echo "Delete? [y|n]"
        read response
        if [ -z "$response" ]; then
            response='n'
        fi
        if [ "$response" == "y" ]; then
            echo "Deleting"
            rm "$linkname"
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
        "sync" )
            sync
            ;;
        "firsttime" )
            firsttime
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

