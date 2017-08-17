" Prevent loading twice
if exists("loaded_pyenv_helper")
  finish
endif
let g:loaded_pyenv_helper = 1

function pyenv#root()
  return system('echo -n $(pyenv root)')
endfunction

function pyenv#path(pyversion)
  return pyenv#root() . "/versions/" . a:pyversion . "/bin/python" 
endfunction
