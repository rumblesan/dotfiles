Vim Plugins and Settings
========================

This is a repo to keep all my vim plugins and settings. This is meant to be used as the .vim folder

Plugins are added as submodules.
.vimrc is meant to be symlinked from ~/.vim/.vimrc
.gvimrc is the same

## Setup

to set this up, do the following

    cd ~
    git clone --recursive git://github.com/notesandvolts/vim-settings.git
    ln -s vim-settings/ .vim
    ln -s .vim/vimrc.symlink .vimrc
    ln -s .vim/gvimrc.symlink .gvimrc

