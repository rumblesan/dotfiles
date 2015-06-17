# ----------------------------------------------------------------------
# HASKELL
# ----------------------------------------------------------------------
# Haskell specific config

# Add cabal bin dir to path
PATH=~/.cabal/bin:$PATH

# If OSX then assume we installed everything with brew cask and add the folder to the path
if [[ "$OSTYPE" == "darwin"* ]]; then
    PATH=$PATH:/opt/homebrew-cask/Caskroom/ghc/7.8.4-r0/ghc-7.8.4.app/Contents/bin
fi

