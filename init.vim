" NeoVim config

let mapleader = ","

" Automatically download Vim-Plug
if empty(glob('~/.config/nvim/autoload/plug.vim'))
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" Install a bunch of stuff using Vim-Plug

call plug#begin()

Plug 'tpope/vim-surround'
Plug 'tpope/vim-fugitive'
Plug 'cespare/vim-toml'
Plug 'dense-analysis/ale'

call plug#end()
