# ----------------------------------------------------------------------
# HASKELL
# ----------------------------------------------------------------------
# Haskell specific config

# Add cabal bin dir to path
PATH=$PATH:~/Library/Haskell/bin

# Simpler way to use hsenv
haskell_env()
{
    if [ -f ./.hsenv/bin/activate ]; then
        echo "Activating HSEnv"
        source ./.hsenv/bin/activate
    else
        echo "No HSEnv present"
    fi
}

