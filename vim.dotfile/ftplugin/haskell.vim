
setlocal shiftwidth=2
setlocal tabstop=2
setlocal softtabstop=2

map <leader>t  :GhcModType<cr>
map <leader>tc :GhcModTypeClear<cr>

map <leader>c  :GhcModCheckAndLintAsync<cr>

let g:ghcmod_open_quickfix_function = 'GhcModQuickFix'
function! GhcModQuickFix()
  :Unite -no-empty quickfix
endfunction


