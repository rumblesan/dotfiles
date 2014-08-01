#! /bin/bash

PRIVATEDIR="$HOME/Library/Application Support/Karabiner"

if [ -d "$PRIVATEDIR" ]; then

    if [ -f "$PRIVATEDIR/private.xml" ]; then
        RESPONSE="n"
        echo -n "private.xml already exists. Should I delete it?: [y/n] "
        read RESPONSE

        if [ $RESPONSE == "y" ]; then
            echo "Deleting private.xml"
            rm "$PRIVATEDIR/private.xml"
        else
            echo "Not deleting private.xml"
            exit 1
        fi

    fi

    ln -s "$HOME/.dotfiles/misc/karabiner/private.xml" "$PRIVATEDIR/private.xml"
else
    echo "$PRIVATEDIR doesn't exist! Is Karabiner installed?"
    exit 1
fi

. import.sh