let g:timelines_default_config = {"socket_name": "default", "target_pane": "timelines:1.2"}
let maplocalleader=","
let g:timelines_no_mappings = 1

if !hasmapto('<Plug>TimeLinesParagraphSend', 'n')
  nmap <buffer> <localleader>e  <Plug>TimeLinesParagraphSend
endif

if !hasmapto('<Plug>TimeLinesRegionSend', 'n')
  vmap <buffer> <localleader>e  <Plug>TimeLinesRegionSend
endif

