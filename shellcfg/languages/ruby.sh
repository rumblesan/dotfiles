# ----------------------------------------------------------------------
# RUBY
# ----------------------------------------------------------------------
# Ruby specific config

if [ -n "$(command -v rbenv)" ]; then
  export PATH="${HOME}/.rbenv/shims:${PATH}"

  function rbenv() {
      unset -f rbenv

      export RBENV_SHELL=zsh
      source '/usr/local/Cellar/rbenv/1.1.2/libexec/../completions/rbenv.zsh'
      command rbenv rehash 2>/dev/null
      rbenv() {
        local command
        command="${1:-}"
        if [ "$#" -gt 0 ]; then
          shift
        fi

        case "$command" in
        rehash|shell)
          eval "$(rbenv "sh-$command" "$@")";;
        *)
          command rbenv "$command" "$@";;
        esac
      }

      rbenv $@
  }
fi
