" Agam's vimrc file 

" Basics {
	set nocompatible 	"explicitly get out of vi-compatible mode
	set noexrc		"avoid reading local .exrc
	set background=dark
	syntax on		"obviously
" }

" Other general settings {
	filetype plugin indent on
	set backspace=indent,eol,start
	set ic	"ignorecase
" }

" UI stuff {
	set cursorcolumn
	set cursorline
	set incsearch
	set laststatus=2
	set lazyredraw
	set linespace=0
	set list
	set listchars=tab:>-,trail:-
	set matchtime=5
	set hlsearch
	set nostartofline
	set number		" line numbering
	set ruler
	set showcmd	"show what we're typing
	set showmatch	"show matching brackets
	set statusline=%F%m%r%h%w[%L][%{&ff}]%y[%p%%][%04l,%04v]
" }

" Formatting {
" }
