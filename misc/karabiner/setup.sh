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
        Link dotfiles private.xml to Library
    import
        Run import script
    firsttime
        Set up karabiner for the first time

Flags:
    -h
        Show usage info
    -f
        Link files without asking user

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
    echo "Karabiner private.xml setup"

    if [ -f "$PRIVATEDIR/private.xml" ]; then

        if [ "$FORCE_LINK" == "y" ]; then
            echo "Deleting current private.xml"
            rm "$PRIVATEDIR/private.xml"
        else

            RESPONSE="n"
            echo -n "private.xml already exists. Should I delete it?: [y/n] "
            read RESPONSE

            if [ $RESPONSE == "y" ]; then
                echo "Deleting current private.xml"
                rm "$PRIVATEDIR/private.xml"
            else
                die "Not deleting current private.xml"
            fi
        fi

    fi

    echo "Linking dotfiles private.xml to $PRIVATEDIR/private.xml"
    ln -s "$DOTFILE_DIR/misc/karabiner/private.xml" "$PRIVATEDIR/private.xml"
}

import()
{
    echo "Import settings to Karabiner"
    . "$IMPORT_SCRIPT"
}

firsttime()
{
    echo "Run firsttime setup"
    setup
    import
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
    "import" )
        import
        ;;
    "firsttime" )
        firsttime
        ;;
    * )
        usage
        ;;
    esac

}

DOTFILE_DIR="$HOME/.dotfiles"
PRIVATEDIR="$HOME/Library/Application Support/Karabiner"
FORCE_LINK="n"
IMPORT_SCRIPT="$DOTFILE_DIR/misc/karabiner/import.sh"

if [ ! -d "$PRIVATEDIR" ]; then
    die "$PRIVATEDIR doesn't exist! Is Karabiner installed?"
fi

echo "Karabiner setup"

while getopts "hf" opt "$@"; do
    case "$opt" in
        h)
            helpinfo
            ;;
        f)
            FORCE_LINK="y"
            echo "Forcing Linking"
            ;;
    esac
done

runaction "${@:$OPTIND}"

