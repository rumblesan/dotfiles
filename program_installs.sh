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
pygments
fabric'

JSCRIPTS='jslint
coffee-script
coffeedoc
coffeelint
grunt-cli'

CASKS='spotify
iterm2
skype
steam
x-quartz
vlc
evernote
google-chrome
size-up'


basic_dev_env()
{
    $DEVBREWS='coreutils git'
    brew update
    for i in $DEVBREWS; do
        brew install $i
    done
    # Install macvim separately with special flags
    brew install macvim --env-std --override-system-vim

    fix_powerline

    $DEVCASKS='x-quartz iterm2 size-up google-chrome'
    brew tap phinze/homebrew-cask
    brew install brew-cask
    for i in $CASKS; do
        brew cask install $i
    done
}

install_brews()
{
    brew update
    for i in $BREWS; do
        brew install $i
    done
    # Install macvim separately with special flags
    brew install macvim --env-std --override-system-vim

    fix_powerline
}

fix_powerline()
{
    # get version numbers for Macvim and python
    MACVIM_VER=`brew ls --versions macvim | sed 's/.* \(.*\)/\1/g'`
    PY_FULL_VER=`brew ls --versions python | sed 's/.* \(.*\)/\1/g'`
    PY_MAJ_VER=`echo $PY_FULL_VER | sed 's/\([0-9][0-9]*\.[0-9][0-9]*\).*/\1/'`

    echo "Fixing Macvim to use brew python"
    echo "Macvim version: $MACVIM_VER"
    echo "Python major version: $PY_MAJ_VER"
    echo "       full version: $PY_FULL_VER"

    # Horrible fix to make macvim link with homebrew python, not system python
    cd /usr/local/Cellar/macvim/$MACVIM_VER/MacVim.app/Contents/MacOS/
    install_name_tool -change /System/Library/Frameworks/Python.framework/Versions/$PY_MAJ_VER/Python /usr/local/Cellar/python/$PY_FULL_VER/Frameworks/Python.framework/Versions/$PY_MAJ_VER/Python MacVim
    install_name_tool -change /System/Library/Frameworks/Python.framework/Versions/$PY_MAJ_VER/Python /usr/local/Cellar/python/$PY_FULL_VER/Frameworks/Python.framework/Versions/$PY_MAJ_VER/Python Vim
}

install_casks()
{
    brew tap phinze/homebrew-cask
    brew install brew-cask
    for i in $CASKS; do
        brew cask install $i
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
        install_casks
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
    "cask" )
        install_casks
        ;;
    "devenv" )
        basic_dev_env
        ;;
    "fixpowerline" )
        fix_powerline
        ;;
    * )
        echo "Need to tell me to install all of this"
        ;;
    esac
}

install $1

