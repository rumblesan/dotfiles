" General settings
set nocompatible
set hidden

" allow external vimrc
set exrc

let g:python3_host_prog = pyenv#path('3.7.2')

" Setup plugins
call plug#begin()

" Defaults
Plug 'tpope/vim-sensible'

" Navigation
Plug 'Shougo/denite.nvim'
Plug 'christoomey/vim-tmux-navigator'

" Styling
Plug 'altercation/vim-colors-solarized'
Plug 'junegunn/rainbow_parentheses.vim'

" General Usability
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'roxma/vim-tmux-clipboard'

" Development
Plug 'w0rp/ale'
Plug 'scrooloose/nerdcommenter'
Plug 'autozimu/LanguageClient-neovim', { 'branch': 'next','do': 'bash install.sh' }

" Language Specific
Plug 'kchmck/vim-coffee-script', { 'for': 'coffee' }
Plug 'munshkr/vim-tidal', { 'for': 'tidal' }
Plug 'neovimhaskell/haskell-vim', { 'for': 'haskell' }
Plug 'derekwyatt/vim-scala', { 'for': 'scala' }
Plug 'derekwyatt/vim-sbt', { 'for': 'sbt.scala' }
Plug 'alunny/pegjs-vim', { 'for': 'pegjs' }
Plug 'fatih/vim-go', { 'for': 'go', 'do': ':GoUpdateBinaries' }
Plug 'Vimjas/vim-python-pep8-indent', { 'for': 'python' }
Plug 'sophacles/vim-processing', { 'for': 'processing' }
Plug 'supercollider/scvim', { 'for': 'supercollider' }
Plug 'leafgarland/typescript-vim', { 'for': 'typescript' }
Plug 'wlangstroth/vim-racket', { 'for': 'racket' }
Plug 'jpalardy/vim-slime'

" My Plugins
Plug '~/src/improviz-vim', { 'for': 'improviz' }
Plug '~/src/vim-timelines', { 'for': 'timelines' }
Plug '~/src/cheapsound/clients/cheapsound.vim', { 'for': 'cheapsound' }
Plug '~/src/glacier/vim', { 'for': 'grains' }

" Add plugins to &runtimepath
call plug#end()

" Display settings
set number
set ruler
set shortmess=I

" Turn mouse mode on
set mouse=a

" Turn alert bells off
set noerrorbells t_vb=
set visualbell

" Turns off swap files and backups
set nobackup
set nowritebackup
set noswapfile

" Actual formatting options
set expandtab
set tabstop=2
set softtabstop=2
set shiftwidth=2

" Spell check
set spelllang=en_gb

" Search settings
set incsearch
"This unsets the "last search pattern" register by hitting return
nnoremap <CR> :noh<CR><CR>

" Colorscheme settings
set background=dark
colorscheme solarized

" Key mappings
" Disable arrow keys
map <up> <nop>
map <down> <nop>
map <left> <nop>
map <right> <nop>
imap <up> <nop>
imap <down> <nop>
imap <left> <nop>
imap <right> <nop>

map ; :

command Cheats :help cheats.txt

" Map leader to easier to use key
let mapleader = ","

" " Copy to clipboard
vnoremap  <leader>y  "+y
nnoremap  <leader>Y  "+yg_
nnoremap  <leader>y  "+y
nnoremap  <leader>yy  "+yy

" " Paste from clipboard
nnoremap <leader>p "+p
nnoremap <leader>P "+P
vnoremap <leader>p "+p
vnoremap <leader>P "+P

" Split settings
set splitbelow
set splitright

" Denite settings
call denite#custom#option('default', 'direction', 'topleft')
call denite#custom#option('default', 'mode', 'normal')
call denite#custom#option('default', 'winheight', 15)
call denite#custom#var('file/rec', 'command', ['ag', '--follow', '--nocolor', '--ignore', '.git', '--nogroup', '--hidden', '-g', ''])

nnoremap <leader>r :<C-u>Denite file/rec<CR>
nnoremap <leader>f :<C-u>Denite file<CR>
nnoremap <leader>b :<C-u>Denite buffer<CR>
nnoremap <leader>g :<C-u>Denite grep<CR>
" Ag command on grep source
call denite#custom#var('grep', 'command', ['ag'])
call denite#custom#var('grep', 'default_opts',
    \ ['-i', '--vimgrep', '--ignore-dir', 'vendor'])
call denite#custom#var('grep', 'recursive_opts', [])
call denite#custom#var('grep', 'pattern_opt', [])
call denite#custom#var('grep', 'separator', ['--'])
call denite#custom#var('grep', 'final_opts', [])

" ALE settings
let g:ale_fix_on_save = 1
let g:ale_fixers = {
    \ '*': ['remove_trailing_lines', 'trim_whitespace'],
    \ }

" Language Server settings
let g:LanguageClient_hoverPreview = "Always"

function! FindWorkspaceSymbol()
  call inputsave()
  let symb = input('Symbol Search: ')
  call inputrestore()
  if symb == ""
    execute "Denite workspaceSymbol"
  else
    execute "Denite -input=" . symb . " workspaceSymbol"
  endif
endfunction

nnoremap <silent> gd  :call LanguageClient#textDocument_definition()<CR>
nnoremap <silent> gh  :call LanguageClient#textDocument_hover()<CR>
nnoremap <silent> gr  :call LanguageClient_textDocument_references()<CR>
nnoremap <silent> gs  :call FindWorkspaceSymbol()<CR>

" Rainbow Parens
augroup rainbow_lisp
  autocmd!
  autocmd FileType lisp,clojure,scheme RainbowParentheses
augroup END

set secure
