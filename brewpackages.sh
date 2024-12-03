#!/usr/bin/env bash

# exit when script tries to use an undeclared variable
set -o nounset
# exit if any command returns a non-zero code
#set -e

die() {
  echo "$*"
  exit 1
}

brewsetup() {
  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
}

basic() {
  local basicbrews=(
    coreutils
    git
    git-lfs
    neovim
    reattach-to-user-namespace
    the_silver_searcher
    tmux
    fzf
  )
  for b in "${basicbrews[@]}"; do
    echo "installing ${b}"
    brew install $b
  done
}

development() {
  local developmentbrews=(
    nodenv
    node-build
    pyenv
    pyenv-virtualenv
  )
  for b in "${developmentbrews[@]}"; do
    echo "installing ${b}"
    brew install $b
  done
}

casks() {
  local caskbrews=(
    alacritty
    docker
    iterm2
    fertigt-slate
    vlc
  )
  brew tap caskroom/cask
  for b in "${caskbrews[@]}"; do
    brew install --cask $b
  done
}

main() {
  basic
  casks
  development
}

main "$@"
