# ----------------------------------------------------------------------
# ALIASES
# ----------------------------------------------------------------------

# Easier navigation
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias ~="cd ~"
alias -- -="cd -"

# Local IP addresses
alias lip="ifconfig -a | perl -nle'/(\d+\.\d+\.\d+\.\d+)/ && print $1'"

# WAN IP Address
alias ip="dig +short myip.opendns.com @resolver1.opendns.com"

if [[ "$OSTYPE" == "darwin"* && -n "$(command -v gls)" ]]; then
  # If coreutils is installed on OSX then GNU ls with --color switch
  local lsbin='gls'
else
  local lsbin='ls'
fi

alias ls="$lsbin -h --color"
alias la="$lsbin -ha --color"
alias ll="$lsbin -hl --color"
alias lal="$lsbin -hal --color"

# Turn on colors for grep
alias grep="grep --color"

# Makes it easy to grep the ps output
alias psgrep="ps aux | grep -v grep | grep"

# cd straight to some useful directories
alias dotfiles="cd ${DOTFILES}"

# makes looking at history easier
alias h="history"

# docker aliases
alias d='docker'
alias dc='docker-compose'
alias kc='kubectl'
alias tf='terraform'

alias e="$EDITOR"
