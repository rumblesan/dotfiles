
let g:ale_linters = {'go': ['gopls']}

autocmd BufWritePre *.go :silent call CocAction('runCommand', 'editor.action.organizeImport')
