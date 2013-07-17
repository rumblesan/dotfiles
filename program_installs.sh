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
sdl
gnu-typist
zsh'

CABALS='cabal-install
test-framework-hunit
quickcheck
haddock
hlint
hdevtools
cabal-dev'

PIPS='flake8
pygments
fabric
git+git://github.com/Lokaltog/powerline
sniffer
MacFSEvents
virtualenv'

JSCRIPTS='jslint
coffee-script
coffeedoc
coffeelint
grunt-cli'

CASKS='spotify
virtualbox
vagrant
iterm2
skype
steam
x-quartz
vlc
evernote
google-chrome
size-up
growlnotify'


basic_dev_env()
{
    $DEVBREWS='coreutils git'
    brew update
    for i in $DEVBREWS; do
        brew install $i
    done
    # Install macvim separately with special flags
    brew install macvim --env-std --override-system-vim

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

set_shell()
{
    chsh zsh
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
        set_shell
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
    * )
        echo "Need to tell me to install all of this"
        ;;
    esac
}

install $1

