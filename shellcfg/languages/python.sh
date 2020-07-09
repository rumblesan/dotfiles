if [ -n "$(command -v pyenv)" ]; then
  export PATH="${HOME}/.pyenv/shims:${PATH}"

  function pyenv() {
      unset -f pyenv

      export PYENV_SHELL=zsh
      source '/usr/local/Cellar/pyenv/1.2.19/libexec/../completions/pyenv.zsh'
      command pyenv rehash 2>/dev/null
      pyenv() {
        local command
        command="${1:-}"
        if [ "$#" -gt 0 ]; then
          shift
        fi

        case "$command" in
        rehash|shell)
          eval "$(pyenv "sh-$command" "$@")";;
        *)
          command pyenv "$command" "$@";;
        esac
      }

      pyenv $@
  }


fi
