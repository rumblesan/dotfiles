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
  /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

}

basic() {
  local basicbrews=(
    coreutils
    "--with-default-names gnu-sed"
    "--with-default-names gnu-tar"
    git
    haskell-stack
    hub
    neovim
    pyenv
    rbenv
    reattach-to-user-namespace
    shfmt
    the_silver_searcher
    tmux
    zsh
  )
  for b in "${basicbrews[@]}"; do
    echo "installing ${b}"
    brew install $b
  done
}

casks() {
  local caskbrews=(
    alfred
    iterm2
    slate
    slack
    vlc
  )
  brew tap caskroom/cask
  for b in "${caskbrews[@]}"; do
    brew cask install $b
  done
}

main() {
  brewsetup
  basic
  casks
}

main "$@"
