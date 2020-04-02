let g:LanguageClient_serverCommands = {
    \ 'haskell': ['hie-wrapper', '--lsp'],
    \ }

let b:ale_fixers = ['brittany']

let b:ale_linters = { 'haskell': [] }
