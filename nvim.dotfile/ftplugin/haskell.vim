let g:LanguageClient_rootMarkers = ['*.cabal', 'stack.yaml']
let g:LanguageClient_serverCommands = {
    \ 'haskell': ['hie-wrapper'],
    \ }

let b:ale_fixers = ['brittany']

let b:ale_linters = { 'haskell': [] }
