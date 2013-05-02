#! /bin/bash

BREWS='bash-completion
bash
colordiff
coreutils
git
giter8
haskell-platform
plt-racket
lcov
node
postgresql
python
rbenv
ruby
ruby-build
scala
sdl'

CABALS='cabal-install
test-framework-hunit
quickcheck
haddock
hlint
hdevtools'

PIPS='flake8
fabric'

JSCRIPTS='jslint
grunt-cli'

install_brews()
{
    brew update
    for i in $BREWS; do
        brew install $i
    done
    # Install macvim separately with special flags
    brew install macvim --env-std --override-system-vim

    # Horrible fix to make macvim link with homebrew python, not system python
    cd /usr/local/Cellar/macvim/7.3-66/MacVim.app/Contents/MacOS/
    install_name_tool -change /System/Library/Frameworks/Python.framework/Versions/2.7/Python /usr/local/Cellar/python/2.7.4/Frameworks/Python.framework/Versions/2.7/Python MacVim
    install_name_tool -change /System/Library/Frameworks/Python.framework/Versions/2.7/Python /usr/local/Cellar/python/2.7.4/Frameworks/Python.framework/Versions/2.7/Python Vim
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
    pip install --upgrade pip
    for i in $PIPS; do
        pip install $i
    done
    # Install powerline separately from github
    pip install --user git+git://github.com/Lokaltog/powerline
}

install_jscripts()
{
    for i in $JSCRIPTS; do
        npm install -g $i
    done
}

install_ruby()
{
    rbenv install 1.9.3-p362
    rbenv rehash
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
        install_ruby
        ;;
    "brew" )
        install_brews
        ;;
    "cabal" )
        install_cabals
        ;;
    "pip" )
        install_pips
        ;;
    "npm" )
        install_jscripts
        ;;
    "ruby" )
        install_ruby
        ;;
    * )
        echo "Need to tell me to install all of this"
        ;;
    esac
}

install $1

