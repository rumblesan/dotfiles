Dotfiles
========

This repo contains all of my important dotfiles so far

Any files with the .symlink extension will be automatically symlinked into my home directory by the install script.
Vim plugins are added as submodules in the vim.symlink/bundle folder with Pathogen handling the details of loading them then.
When cloneing the repo, remember to use the --recursive switch, makes life much easier

Setup
-----

    git clone --recursive git://github.com/rumblesan/dotfiles.git ~/.dotfiles
    cd ~/.dotfiles
    ./install.py

Notes
-----

The vimproc plugin needs to be compiled when it's installed.

    cd ~/.dotfiles/vim.dotfile/bundle/vimproc
    make

