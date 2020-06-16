
if expand("%:e") == "trax"
  let g:slime_dont_ask_default = 1

  function SlimeOverride_EscapeText_lua(text)
    return shellescape(a:text, 1)
  endfunction

  function SlimeOverrideSend(config, text)
    silent execute '!oscsend' 'localhost' '7770' '/run/code' 's' a:text
  endfunction
endif
