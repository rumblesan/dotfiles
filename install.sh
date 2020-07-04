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
    echo "Basic setup"
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

    if [ ! -d ~/.ssh ]; then
      mkdir -p ~/.ssh
    fi
    linkfile "$DOTFILE_DIR/ssh/config" ~/.ssh/config

    if [ ! -d ~/.config ]; then
      mkdir -p ~/.config
    fi
    linkfile "$DOTFILE_DIR/nvim.dotfile" ~/.config/nvim

    if [ ! -d ~/src ]; then
      mkdir -p ~/src
    fi
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
    echo "Cleanup symlinks"
    local link=""
    local symlinks=`find ~/ -maxdepth 1 -type l`
    for link in $symlinks; do
        if [ ! -e "$link" ]; then
            deletefile "$link"
        fi
    done
    if [ ! -d ~/.ssh ]; then
      deletefile ~/.ssh/config
    fi
    if [ ! -d ~/.config ]; then
      deletefile ~/.config/nvim
    fi
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

runaction()
{
    if [ "$DOTFILE_DIR" != "$PWD" ]; then
        die "This script needs to be run from the dotfiles directory:  $DOTFILE_DIR"
    fi

    action=$( printf "%s\n" "$1" | tr 'A-Z' 'a-z' )

    case "$action" in
    "setup" )
        setup
        ;;
    "cleanup" )
        cleanup
        ;;
    * )
        usage
        ;;
    esac

}

DOTFILE_DIR=~/.dotfiles
PWD="`pwd`"

FORCE_DELETE="n"

echo "Dotfiles setup"

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
