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

alias ls="ls -h --color"
alias la="ls -ha --color"
alias ll="ls -hl --color"
alias lal="ls -hal --color"

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
