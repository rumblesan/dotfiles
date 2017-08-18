if exists('g:loaded_filebrowse')
  finish
endif
let g:loaded_filebrowse = 1

function! filebrowse#browse()
  execute("Sexplore " . getcwd())
endfunction
