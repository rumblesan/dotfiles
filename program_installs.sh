#! /bin/bash

BREWS='bash-completion
colordiff
coreutils
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
todo-txt
wget'

CABALS='cabal-install
hdevtools'

PIPS='flake8
fabric'

JSCRIPTS='jslint'

install_brews()
{
    brew update
    for i in $BREWS; do
        brew install $i
    done
}

install_cabals()
{
    cabal update
    for i in $CABALS; do
        cabal install $i
    done
}

install_pips()
{
    for i in $PIPS; do
        pip install $i
    done
}

install_jscripts()
{
    for i in $JSCRIPTS; do
        npm install $i
    done
}

install()
{
    action=$( printf "%s\n" "$1" | tr 'A-Z' 'a-z' )

    case $action in
    "install" )
        install_brews
        install_cabals
        install_pips
        install_jscripts
        ;;
    * )
        echo "Need to tell me to install all of this"
        ;;
    esac
}

install $1

