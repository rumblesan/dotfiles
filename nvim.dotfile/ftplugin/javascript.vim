" check for node_modules version of Prettier and fallback to global
let s:prettierprg = findfile('node_modules/.bin/prettier', '.;')
if !executable(s:prettierprg)
    let s:prettierprg = exepath('prettier')
endif

let g:neoformat_javascript_prettier = {
        \ 'exe': s:prettierprg,
        \ 'args': ['--stdin', '--stdin-filepath', '%:p'],
        \ 'stdin': 1,
        \ }

" check for node_modules version of Standard and fallback to global
let s:standardprg = findfile('node_modules/.bin/standard', '.;')
if !executable(s:standardprg)
    echom "not executable"
    let s:standardprg = exepath('standard')
endif

let g:neoformat_javascript_standard = {
        \ 'exe': s:standardprg,
        \ 'args': ['--stdin','--fix'],
        \ 'stdin': 1,
        \ }

let g:neoformat_enabled_javascript = get(g:, 'neoformat_enabled_javascript', ['prettier'])
