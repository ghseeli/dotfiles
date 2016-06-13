" Run pathogen
execute pathogen#infect()

" new stuff
set wildmenu
set hlsearch

" Numbering
set number          " Show line numbers
set ruler           " Show column number

" Tab behavior and spacing
set tabstop=4       " The width of a TAB is set to 4.
                    " Still it is a \t. It is just that
                    " Vim will interpret it to be having
                    " a width of 4.
set shiftwidth=4    " Indents will have a width of 4
set softtabstop=4   " Sets the number of columns for a TAB
set expandtab       " Expand TABs to spaces

inoremap jk <ESC>
let mapleader = "\<Space>"
filetype plugin indent on
syntax on
set encoding=utf-8
