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
    direnv
    git
    gnupg2
    haskell-stack
    hub
    kubectl
    neovim
    nodenv
    pyenv
    pyenv-virtualenv
    reattach-to-user-namespace
    shfmt
    tidy_html5
    the_silver_searcher
    tmux
    zsh
    zsh-completions
  )
  for b in "${basicbrews[@]}"; do
    echo "installing ${b}"
    brew install $b
  done
}

casks() {
  local caskbrews=(
    alfred
    docker
    iterm2
    postman
    slate
    slack
    spotify
    vlc
  )
  brew tap caskroom/cask
  for b in "${caskbrews[@]}"; do
    brew cask install $b
  done
}

work() {
  local workbrews=(
    heroku
    go
    dep
  )
  for b in "${workbrews[@]}"; do
    echo "installing ${b}"
    brew install $b
  done
}

main() {
  brewsetup
  basic
  work
  casks
}

main "$@"
