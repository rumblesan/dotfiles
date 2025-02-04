# ----------------------------------------------------------------------
# PYTHON
# ----------------------------------------------------------------------
# Python specific config

if [ -n "$(command -v pyenv)" ]; then
  export PYENV_VIRTUALENV_DISABLE_PROMPT=1
  export PYENV_ROOT="$HOME/.pyenv"

  pyenv() {
      eval "$(command pyenv init - zsh)"

      pyenv "$@"
  }

fi
