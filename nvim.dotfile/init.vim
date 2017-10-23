" General settings
set nocompatible

let g:python3_host_prog = pyenv#path('3.5.0')

" Setup plugins
call plug#begin()

" Defaults
Plug 'tpope/vim-sensible'

" Navigation
Plug 'Shougo/denite.nvim'
Plug 'christoomey/vim-tmux-navigator'
Plug 'tpope/vim-vinegar'

" Styling
Plug 'altercation/vim-colors-solarized'
Plug 'junegunn/rainbow_parentheses.vim'

" General Usability
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'

" Development
Plug 'neomake/neomake'
Plug 'benjie/neomake-local-eslint.vim', { 'for': 'javascript' }
Plug 'sbdchd/neoformat'
Plug 'janko-m/vim-test'
Plug 'scrooloose/nerdcommenter'

" Language Specific
Plug 'kchmck/vim-coffee-script', { 'for': 'coffee' }
Plug 'munshkr/vim-tidal', { 'for': 'haskell.tidal' }
Plug 'neovimhaskell/haskell-vim', { 'for': 'haskell' }
Plug 'derekwyatt/vim-scala', { 'for': 'scala' }
Plug 'derekwyatt/vim-sbt', { 'for': 'sbt.scala' }
Plug 'alunny/pegjs-vim'
Plug 'martinda/Jenkinsfile-vim-syntax'

" My Plugins
Plug '~/src/improviz-client.vim', { 'for': 'improviz' }

" Add plugins to &runtimepath
call plug#end()

" Display settings
set number
set relativenumber
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
call denite#custom#var('file_rec', 'command', ['ag', '--follow', '--nocolor', '--ignore', '.git', '--nogroup', '--hidden', '-g', ''])

nnoremap <leader>r :<C-u>Denite file_rec<CR>
nnoremap <leader>f :<C-u>call filebrowse#browse()<CR>
nnoremap <leader>b :<C-u>Denite buffer<CR>
call denite#custom#map('insert', '<C-s>', '<denite:do_action:split>', 'noremap')
call denite#custom#map('normal', 's', '<denite:do_action:split>', 'noremap')
call denite#custom#map('insert', '<C-v>', '<denite:do_action:vsplit>', 'noremap')
call denite#custom#map('normal', 'v', '<denite:do_action:vsplit>', 'noremap')

" Rainbow Parens
augroup rainbow_lisp
  autocmd!
  autocmd FileType lisp,clojure,scheme RainbowParentheses
augroup END

" Test
nmap <silent> <leader>tt :TestNearest<CR>
nmap <silent> <leader>tf :TestFile<CR>
nmap <silent> <leader>ta :TestSuite<CR>
nmap <silent> <leader>tl :TestLast<CR>
nmap <silent> <leader>tg :TestVisit<CR>

" Prettier / Neoformat
autocmd FileType javascript setlocal formatprg=npm\ run\ prettier\ --silent\ --\ --stdin

" Neoformat settings
autocmd BufWritePre * Neoformat
autocmd! BufWritePost * Neomake
let g:neoformat_only_msg_on_error = 1

" Use formatprg when available
let g:neoformat_try_formatprg = 1

" Neomake settings
let g:neomake_open_list = 2

