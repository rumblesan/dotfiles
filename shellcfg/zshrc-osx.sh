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

  # if brew and coreutils are installed, then add the gnubin to the path
  if [ -n $(command -v gls) ]; then

    PATH="$BREWPATH/opt/coreutils/libexec/gnubin:$PATH"
    MANPATH="$BREWPATH/opt/coreutils/libexec/gnuman:$MANPATH"
    CPATH="/usr/local"
    C_INCLUDE_PATH="/usr/local/include"

  fi

fi
