# ----------------------------------------------------------------------
# HASKELL
# ----------------------------------------------------------------------
# Haskell specific config

# Add cabal bin dir to path
if [[ "$OSTYPE" == "darwin"* ]]
then
    PATH=~/Library/Haskell/bin:$PATH
else
    PATH=~/.cabal/bin:$PATH
fi

