let b:ale_fixers = { 'python': ['black'] }

let g:ale_python_black_options = '--line-length 99'
let g:ale_python_flake8_options = '--max-line-length 99'

let b:ale_linters = { 'python': ['flake8'] }
