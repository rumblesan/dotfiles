let g:neoformat_html_html5 = {
        \ 'exe': 'tidy',
        \ 'args': ['-quiet',
        \          '--indent auto',
        \          '--indent-spaces ' . shiftwidth(),
        \          '--vertical-space yes',
        \          '--tidy-mark no',
        \          '-wrap ' . &textwidth
        \         ]
        \ }

let g:neoformat_enabled_html = ['html5']
