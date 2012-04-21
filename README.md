Vim Plugins and Settings
========================

This is a repo to keep all my vim plugins and settings. This is meant to be used as the .vim folder

Plugins are added as submodules.
.vimrc is meant to be symlinked from ~/.vim/.vimrc

## Setup

to set this up, do the following

    cd ~
    git clone git://github.com/notesandvolts/vim-settings.git
    mv vim-settings .vim
    ln -s .vim/.vimrc .vimrc
    cd .vim
    git submodule init
    git submodule update
    cd bundle/pyflakes
    git submodule init
    git submodule update

