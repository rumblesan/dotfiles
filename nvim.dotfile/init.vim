" General settings
set nocompatible

" TODO use pyenv root to generate this
let g:python3_host_prog="/opt/boxen/pyenv/versions/3.6.0/bin/python"

" Setup plugins
call plug#begin()

Plug 'Shougo/denite.nvim'
Plug 'christoomey/vim-tmux-navigator'

Plug 'vim-airline/vim-airline'

Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'

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

" Indentation settings
set autoindent

" Actual formatting options
set expandtab
set tabstop=4
set shiftwidth=4

" Search settings
set incsearch

" Colorscheme settings
set background=dark

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

" Split settings
set splitbelow
set splitright

" Denite settings
call denite#custom#option('default', 'direction', 'topleft')
call denite#custom#option('default', 'mode', 'normal')
call denite#custom#option('default', 'winheight', 15)
nnoremap <leader>r :<C-u>Denite file_rec<CR>
nnoremap <leader>b :<C-u>Denite buffer<CR>

" Vim airline settings
set guifont=Menlo\ for\ Powerline:h12
let g:airline_powerline_fonts = 1
let g:airline_mode_map = {
            \ '__' : '-',
            \ 'n'  : 'N',
            \ 'i'  : 'I',
            \ 'R'  : 'R',
            \ 'c'  : 'C',
            \ 'v'  : 'V',
            \ 'V'  : 'V',
            \ 's'  : 'S',
            \ 'S'  : 'S',
            \ }
