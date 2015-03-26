# ----------------------------------------------------------------------
# HASKELL
# ----------------------------------------------------------------------
# Haskell specific config

# Add cabal bin dir to path
if [[ "$OSTYPE" == "darwin"* ]]
then
    PATH=~/Library/Haskell/bin:$PATH:/opt/homebrew-cask/Caskroom/ghc/7.8.4-r0/ghc-7.8.4.app/Contents/bin
else
    PATH=~/.cabal/bin:$PATH
fi

