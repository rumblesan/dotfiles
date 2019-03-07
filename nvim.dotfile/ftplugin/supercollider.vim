
let g:scFlash = 1
let maplocalleader=","

function! SCPerformanceBindings()
  nnoremap <buffer> <localleader>h :call SClangHardstop()<cr>
  nnoremap <buffer> <c-h> :call SClangHardstop()<cr>

  nnoremap <buffer> <localleader>e :call SClang_line()<cr>
  nnoremap <buffer> <localleader>a :call SClang_block()<cr>
endfunction

