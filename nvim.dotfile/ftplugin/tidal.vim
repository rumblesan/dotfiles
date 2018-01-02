let g:tidal_default_config = {"socket_name": "default", "target_pane": "tidal:1.2"}
let maplocalleader=","
let g:tidal_no_mappings = 1

if !hasmapto('<Plug>TidalLineSend', 'n')
  nmap <buffer> <localleader>e  <Plug>TidalLineSend
endif

nnoremap <buffer> <localleader>h :TidalHush<cr>
nnoremap <buffer> <c-h> :TidalHush<cr>
