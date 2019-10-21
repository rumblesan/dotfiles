let g:LanguageClient_serverCommands = {
    \ 'haskell': ['hie-wrapper -r ./src'],
    \ }

let g:LanguageClient_rootMarkers = ['*.cabal', 'stack.yaml']

let g:ale_linters = {'haskell': ['hie']}

