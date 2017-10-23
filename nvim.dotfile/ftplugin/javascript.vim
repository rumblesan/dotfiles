runtime plugin/neomake-local-eslint.vim

let g:neoformat_javascript_prettier = {
        \ 'exe': GetNpmBin('prettier'),
        \ 'args': ['--stdin', '--stdin-filepath', '%:p'],
        \ 'stdin': 1,
        \ }

let g:neoformat_enabled_javascript = ['prettier']
