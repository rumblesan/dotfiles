let g:LanguageClient_serverCommands = {
    \ 'rust': ['~/.cargo/bin/rustup', 'run', 'stable', 'rls'],
    \ }

let b:ale_fixers = ['rustfmt']

let g:ale_linters = { 'rust': [] }
