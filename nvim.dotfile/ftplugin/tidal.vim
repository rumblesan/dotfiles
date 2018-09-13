let g:tidal_default_config = {"socket_name": "default", "target_pane": "tidal:1.2"}
let maplocalleader=","
let g:tidal_no_mappings = 1

if !hasmapto('<Plug>TidalParagraphSend', 'n')
  nmap <buffer> <localleader>e  <Plug>TidalParagraphSend
endif

if !hasmapto('<Plug>TidalRegionSend', 'n')
  vmap <buffer> <localleader>e  <Plug>TidalRegionSend
endif

nnoremap <buffer> <localleader>h :TidalHush<cr>
nnoremap <buffer> <c-h> :TidalHush<cr>

