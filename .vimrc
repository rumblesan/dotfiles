runtime bundle/vim-pathogen/autoload/pathogen.vim
call pathogen#infect()
syntax on
filetype plugin indent on

set number
set ruler
colorscheme evening

set visualbell       "use visual instead of audible bell

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

