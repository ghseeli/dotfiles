" Run pathogen
execute pathogen#infect()

" new stuff
set wildmenu
set hlsearch

" Numbering
set relativenumber          " Show line numbers
set ruler           " Show column number

function! NumberToggle()    "Toggle between absolute and relative line numbers
  if(&relativenumber == 1)
    set number
  else
    set relativenumber
  endif
endfunc

nnoremap <C-n> :call NumberToggle()<cr>

:au FocusLost * :set number " Return to absolute numbers when vim loses focus
:au FocusGained * :set relativenumber

autocmd InsertEnter * :set number " Insert mode uses absolute numbers since 
                                  " movement does not happen in insert mode.
autocmd InsertLeave * :set relativenumber

" Tab behavior and spacing
set tabstop=4       " The width of a TAB is set to 4.
                    " Still it is a \t. It is just that
                    " Vim will interpret it to be having
                    " a width of 4.
set shiftwidth=4    " Indents will have a width of 4
set softtabstop=4   " Sets the number of columns for a TAB
set expandtab       " Expand TABs to spaces

" Common keybindings
inoremap jk <ESC>
nnoremap ; :
let mapleader = "\<Space>"
filetype plugin indent on
syntax on
set encoding=utf-8
