# OSX specific dev settings

# ----------------------------------------------------------------------
# ULIMIT
# ----------------------------------------------------------------------

# increase ulimit
ulimit -n 2560

# ----------------------------------------------------------------------
# PATH
# ----------------------------------------------------------------------

if [ -n $(command -v brew) ]; then
  export BREWPATH=$(brew --prefix)

  export CPATH=/opt/homebrew/include
  export LIBRARY_PATH=/opt/homebrew/lib
  export C_INCLUDE_PATH="/usr/local/include"

  # if brew and coreutils are installed, then add the gnubin to the path
  if [ -n $(command -v gls) ]; then
    export PATH="$BREWPATH/opt/coreutils/libexec/gnubin:$PATH"
    export MANPATH="$BREWPATH/opt/coreutils/libexec/gnuman:$MANPATH"
  fi
fi
