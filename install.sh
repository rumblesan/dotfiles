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
    for FILE in *.symlink; do
        FILE_NAME=$DOTFILE_DIR/$FILE
        LINK_NAME=~/`echo ".$FILE" | sed "s/\.symlink//"`
        echo "Creating $LINK_NAME"
        if [ -e $LINK_NAME ]; then
            echo "$LINK_NAME already exists!"
            if [ $FORCE_DELETE == "y" ]; then
                echo "Deleting"
                rm $LINK_NAME
                ln -s $FILE_NAME $LINK_NAME
            else
                RESPONSE="n"
                echo "Delete? [y|n]"
                read RESPONSE
                if [ ! -z $RESPONSE ]; then
                    RESPONSE='n'
                fi
                if [ $RESPONSE == "y" ]; then
                    echo "Deleting"
                    rm $LINK_NAME
                    ln -s $FILE_NAME $LINK_NAME
                else
                    echo "Skipping"
                fi
            fi
        else
            ln -s $FILE_NAME $LINK_NAME
        fi
    done
}

cleanup()
{
    for FILE in *.symlink; do
        LINK_NAME=~/`echo ".$FILE" | sed "s/\.symlink//"`
        echo $LINK_NAME
        if [ $FORCE_DELETE == "y" ]; then
                echo "Deleting"
                rm $LINK_NAME
        else
            RESPONSE="n"
            echo "Delete? [y|n]"
            read RESPONSE
            if [ -z $RESPONSE ]; then
                RESPONSE='n'
            fi
            if [ $RESPONSE == "y" ]; then
                echo "Deleting"
                rm $LINK_NAME
            else
                echo "Skipping"
            fi
        fi
    done
}

update()
{
    git submodule foreach git checkout master
    git submodule foreach git pull origin master
    git submodule update
}

runaction()
{
    if [ $DOTFILE_DIR == $PWD ]; then

        action=$( printf "%s\n" "$1" | tr 'A-Z' 'a-z' )

        case $action in
        "setup" )
            setup
            ;;
        "cleanup" )
            cleanup
            ;;
        "update" )
            update
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
    case $opt in
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

