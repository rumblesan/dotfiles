# ----------------------------------------------------------------------
# JAVASCRIPT
# ----------------------------------------------------------------------
# Javascript specific config

if [ -n "$(command -v nodenv)" ]; then
  export PATH="${HOME}/.nodenv/shims:${PATH}"

  function nodenv() {
      unset -f nodenv

      export NODENV_SHELL=zsh
      local brew_path=`brew --prefix`
      local version='1.4.0'
      source "${brew_path}/Cellar/nodenv/${version}/completions/nodenv.zsh"
      command nodenv rehash 2>/dev/null
      nodenv() {
        local command
        command="${1:-}"
        if [ "$#" -gt 0 ]; then
          shift
        fi

        case "$command" in
        rehash|shell)
          eval "$(nodenv "sh-$command" "$@")";;
        *)
          command nodenv "$command" "$@";;
        esac
      }

      nodenv $@
  }
fi
