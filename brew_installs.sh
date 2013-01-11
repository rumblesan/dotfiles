#! /bin/bash

programs='bash-completion
colordiff
coreutils
ghc
git
giter8
haskell-platform
irssi
lcov
macvim
mysql
node
postgresql
python
rbenv
ruby
ruby-build
sbt
scala
sdl
subversion
todo-txt
wget'

setup()
{
    for i in $programs; do
        brew install $i
    done
}

display()
{
    for i in $programs; do
        brew list $i
    done
}

runaction()
{
    action=$( printf "%s\n" "$1" | tr 'A-Z' 'a-z' )

    case $action in
    "install" )
        setup
        ;;
    "show" )
        display
        ;;
    * )
        echo "Need to tell me to install all of this"
        ;;
    esac
}

runaction $1

