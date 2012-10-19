#!/bin/bash

# Easier navigation: .., ..., ~ and -
alias ..="cd .."
alias ...="cd ../.."
alias ~="cd ~" # `cd` is probably faster to type though
alias -- -="cd -"

# Local IP addresses
alias lip="ifconfig -a | perl -nle'/(\d+\.\d+\.\d+\.\d+)/ && print $1'"

# Turn on colors for grep
alias grep="grep --color"

# By default, use normal ls and --color switch
alias ls="ls -h --color"
alias la="ls -ha --color"
alias ll="ls -hl --color"
alias lal="ls -hal --color"

# Makes it easy to grep the ps output
alias psgrep="ps -A | grep -v grep | grep"

# cd straight to some useful directories
alias dotfiles="cd ${DOTFILES}"
alias repos="cd ${HOME}/repositories"

# todo.sh: https://github.com/ginatrapani/todo.txt-cli
function t() { 
  if [ $# -eq 0 ]; then
    todo.sh ls
  else
    todo.sh $*
  fi
}

function n() { 
  if [ $# -eq 0 ]; then
    notes.sh ls
  else
    notes.sh $*
  fi
}

function c8() { 
  if [ $# -eq 0 ]; then
    com8.sh ls
  else
    com8.sh $*
  fi
}


