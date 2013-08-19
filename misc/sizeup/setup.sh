#! /bin/bash


PLISTDIR="$HOME/Library/Preferences"
PLISTFILE="com.irradiatedsoftware.SizeUp.plist"

if [ -d "$PLISTDIR" ]; then

    if [ -f "$PLISTDIR/$PLISTFILE" ]; then
        RESPONSE="n"
        echo -n "$PLISTFILE already exists. Should I delete it?: [y/n] "
        read RESPONSE

        if [ $RESPONSE == "y" ]; then
            echo "Deleting $PLISTFILE"
            rm "$PLISTDIR/$PLISTFILE"
        else
            echo "Not deleting $PLISTFILE"
            exit 1
        fi

    fi

    echo "Linking plist file"
    ln -s "$HOME/.dotfiles/misc/sizeup/$PLISTFILE" "$PLISTDIR/$PLISTFILE"
else
    echo "$PLISTDIR doesn't exist!"
    exit 1
fi

