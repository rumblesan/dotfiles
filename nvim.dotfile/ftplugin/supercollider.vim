
let g:scFlash = 1
let g:scSplitDirection = "v"
let g:scSplitSize = "25"

nnoremap <buffer> <leader>h :call SClangHardstop()<cr>

nnoremap <buffer> <leader>e :call SClang_line()<cr>
nnoremap <buffer> <leader>a :call SClang_block()<cr>

function SynthDef()
  call append(".", [
  \ "(",
  \ "  SynthDef(\\newsynth, {arg out=0, freq=60, gate=1, amp=1.0;",
  \ "    var osc;",
  \ "    Out.ar(out, osc);",
  \ "  }).add;",
  \ ")",
  \ "",
  \])
endfunction

command! SynthDef call SynthDef()

function Pdef()
  call append(".", [
  \ "(",
  \ "  Pdef(\\pat,",
  \ "    Pbind(",
  \ "      \\instrument, \\changeme,",
  \ "      \\degree, 1,",
  \ "      \\dur, 1.0,",
  \ "      \\ctranspose, 0,",
  \ "      \\amp, 1.0,",
  \ "    )",
  \ "  );",
  \ ")",
  \ "",
  \ "Pdef(\\pat).play;",
  \ "Pdef(\\pat).stop;",
  \ "",
  \])
endfunction

command! Pdef call Pdef()

function Pbind()
  call append(".", [
  \ "(",
  \ "  ~pat = Pbind(",
  \ "    \\instrument, \\changeme,",
  \ "    \\degree, 1,",
  \ "    \\dur, 1.0,",
  \ "    \\ctranspose, 0,",
  \ "    \\amp, 1.0,",
  \ "  );",
  \ ")",
  \ "",
  \ "~pat.play;",
  \ "~pat.stop;",
  \ "",
  \])
endfunction

command! Pbind call Pbind()
