# ----------------------------------------------------------------------
# RUST
# ----------------------------------------------------------------------
# Rust specific config

# put ~/.cargo/bin on PATH if you have it
if [ -d $HOME/.cargo/bin ]; then
  PATH="$PATH:$HOME/.cargo/bin"
fi

# source cargo env file if it exists
[ -f $HOME/.cargo/env ] && source $HOME/.cargo/env
