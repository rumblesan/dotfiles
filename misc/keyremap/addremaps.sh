#! /bin/bash


PRIVATEDIR="$HOME/Library/Application Support/KeyRemap4MacBook"
PLISTDIR="$HOME/Library/Preferences"

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

    ln -s "$HOME/.dotfiles/misc/keyremap/private.xml" "$PRIVATEDIR/private.xml"
else
    echo "$PRIVATEDIR doesn't exist! Is KeyRemap4MacBook installed?"
    exit 1
fi

if [ -d "$PLISTDIR" ]; then

    if [ -f "$PLISTDIR/org.pqrs.KeyRemap4MacBook.plist" ]; then
        RESPONSE="n"
        echo -n "org.pqrs.KeyRemap4MacBook.plist already exists. Should I delete it?: [y/n] "
        read RESPONSE

        if [ $RESPONSE == "y" ]; then
            echo "Deleting org.pqrs.KeyRemap4MacBook.plist"
            rm "$PLISTDIR/org.pqrs.KeyRemap4MacBook.plist"
        else
            echo "Not deleting org.pqrs.KeyRemap4MacBook.plist"
            exit 1
        fi

    fi

    ln -s "$HOME/.dotfiles/misc/keyremap/org.pqrs.KeyRemap4MacBook.plist" "$PLISTDIR/org.pqrs.KeyRemap4MacBook.plist"
else
    echo "$PLISTDIR doesn't exist! Is KeyRemap4MacBook installed?"
    exit 1
fi

