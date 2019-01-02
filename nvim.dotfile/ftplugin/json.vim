" check for node_modules version of Prettier and fallback to global
let s:prettierprg = findfile('node_modules/.bin/prettier', '.;')
if !executable(s:prettierprg)
    let s:prettierprg = exepath('prettier')
endif

let g:neoformat_json_prettier = {
        \ 'exe': s:prettierprg,
        \ 'args': ['--stdin', '--stdin-filepath', '%:p'],
        \ 'stdin': 1,
        \ }

" check for node_modules version of Standard and fallback to global
let s:standardprg = findfile('node_modules/.bin/standard', '.;')
if !executable(s:standardprg)
    let s:standardprg = exepath('standard')
endif

let g:neoformat_json_standard = {
        \ 'exe': s:standardprg,
        \ 'args': ['--stdin','--fix'],
        \ 'stdin': 1,
        \ }

let g:neoformat_enabled_json = get(g:, 'neoformat_enabled_json', ['prettier'])