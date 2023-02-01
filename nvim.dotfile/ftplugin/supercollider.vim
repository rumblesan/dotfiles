
let g:scFlash = 1
let g:scSplitDirection = "v"
let g:scSplitSize = "25"

nnoremap <buffer> <leader>h :call SClangHardstop()<cr>

nnoremap <buffer> <leader>e :call SClang_line()<cr>
nnoremap <buffer> <leader>a :call SClang_block()<cr>
