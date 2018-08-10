" check for node_modules version of Prettier and fallback to global
let s:prettierprg = findfile('node_modules/.bin/prettier', '.;')
if !executable(s:prettierprg)
    let s:prettierprg = exepath('prettier')
endif

let g:neoformat_css_prettier = {
        \ 'exe': s:prettierprg,
        \ 'args': ['--stdin', '--stdin-filepath', '%:p'],
        \ 'stdin': 1,
        \ }