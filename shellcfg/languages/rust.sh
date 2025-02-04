# ----------------------------------------------------------------------
# RUST
# ----------------------------------------------------------------------
# Rust specific config

if [[ -d $HOME/.cargo ]]; then
  . "$HOME/.cargo/env"
  export PATH="$HOME/.cargo/bin:$PATH"
fi
