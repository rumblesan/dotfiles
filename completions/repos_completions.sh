#!/bin/bash
_complete_repos ()
{
        COMPREPLY=()
        cur="${COMP_WORDS[COMP_CWORD]}"
        comp_repo_folders=`/bin/ls ${REPODIR}`
        COMPREPLY=( $(compgen -W "${comp_repo_folders}" -- $cur))
        return 0
}
complete -F _complete_repos repos
