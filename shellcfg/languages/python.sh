# ----------------------------------------------------------------------
# PYTHON
# ----------------------------------------------------------------------
# Python specific config

# settins for virtual env wrapper
export WORKON_HOME=$HOME/.virtualenvs
export PROJECT_HOME=$REPODIR

# disables prompt mangling in virtual_env/bin/activate
export VIRTUAL_ENV_DISABLE_PROMPT=1

# ignore python bytecode
export FIGNORE="$FIGNORE:.pyc"

if [ -n "$(command -v pyenv)" ]; then
  eval "$(pyenv init -)"
  eval "$(pyenv virtualenv-init -)"
fi
