#! /usr/bin/env bash

# setup the necessary cabal packages

cabal update

# these are all packages that should be globally installed
# everything else should be in an hsenv environment
packages='hdevtools
hsenv'

for p in $packages; do
    cabal install $p
done

