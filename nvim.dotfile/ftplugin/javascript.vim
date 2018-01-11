" check for node_modules version of Prettier and fallback to global
let s:formatprg = findfile('node_modules/.bin/prettier', '.;')
if !executable(s:formatprg)
    let s:formatprg = exepath('prettier')
endif

let g:neoformat_javascript_prettier = {
        \ 'exe': s:formatprg,
        \ 'args': ['--stdin', '--stdin-filepath', '%:p'],
        \ 'stdin': 1,
        \ }

let g:neoformat_enabled_javascript = ['prettier']
