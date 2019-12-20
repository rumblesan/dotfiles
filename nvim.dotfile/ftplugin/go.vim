let g:ale_go_gometalinter_executable = 'gometalinter'
let g:ale_go_gometalinter_options = '--fast --exclude=should\ have\ comment'
let g:ale_linters = {'go': ['gometalinter']}
let g:go_fmt_command = "goimports"

let g:go_fmt_fail_silently = 1

let g:go_highlight_build_constraints = 1
let g:go_highlight_extra_types = 1
let g:go_highlight_fields = 1
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_operators = 1
let g:go_highlight_structs = 1
let g:go_highlight_types = 1
