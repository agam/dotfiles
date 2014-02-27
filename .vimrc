" Agam's vimrc file 

" We like plugins
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" All bundles go here
Bundle 'cemerick/piggieback'
Bundle 'guns/vim-clojure-static'
Bundle 'tpope/vim-classpath'
Bundle 'tpope/vim-fireplace'

" Basics {
  set nocompatible 	"explicitly get out of vi-compatible mode
  set noexrc		"avoid reading local .exrc
  set background=dark
  syntax on		"obviously
" }

" Other general settings {
  filetype plugin indent on
  set autowrite
  set backspace=indent,eol,start
  set confirm
  set foldmethod=marker
  set hlsearch
  set ignorecase
  set incsearch
  set showcmd
  set smartcase
  set noswapfile
" }

" UI stuff {
  colorscheme koehler
  set cursorline
  set laststatus=2
  set lazyredraw
  set linespace=0
  set list
  set listchars=tab:>-,trail:-
  set matchtime=5
  set nostartofline
  set number		" line numbering
  set numberwidth=1
  set ruler
  set showcmd	"show what we're typing
  set showmatch	"show matching brackets
  set statusline=%F%m%r%h%w[%L][%{&ff}]%y[%p%%][%04l,%04v]
" }

" Process {
 " We live in a world with version control,
" so get rid of swaps and backups
  set nobackup
  set nowritebackup
  set noswapfile
" }

" Personal customizations {
  " Navigate error list without :cnext and :cprevious
  nmap <C-x>L :clist<CR>
  nmap <C-x>P :cprevious<CR>
  nmap <C-x>N :cnext<CR>

  " No tabs !!! Tabs are evil !!!
  set tabstop=2 shiftwidth=2 expandtab

  " Timestamp
  nmap <C-c><C-t> o@<C-R>=strftime("%H:%M")<CR><CR><CR>
"}

" Lisp config {
    let g:lisp_rainbow=1
" }

