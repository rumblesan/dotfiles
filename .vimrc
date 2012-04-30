runtime bundle/vim-pathogen/autoload/pathogen.vim
call pathogen#infect()
syntax on
filetype plugin indent on

set number
set ruler
colorscheme evening

set lines=60
set columns=95

set visualbell       "use visual instead of audible bell

set guioptions-=T    "remove toolbar
set guioptions+=a    "use global copy/paste buffer

"turns off the swap file stuff
set nobackup
set nowritebackup
set noswapfile

set autoindent
set smartindent

"stop files folding when they're opened
set foldlevelstart=99

"actual formatting options
set expandtab
set tabstop=4
set shiftwidth=4

