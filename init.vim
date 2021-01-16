
" Some common settings
set relativenumber
let mapleader = ","

" Fast access to buffers
nnoremap <leader>n :bn<cr>
nnoremap <leader>m :bp<cr>

" Quick escape
imap jj <esc>
cmap jj <esc>
vmap jj <esc>

" Quick save
noremap <leader>w :w<cr>

" Quick close
noremap <leader>q :q<cr>

" Quick config-load
noremap <leader><leader> :source ~/.config/nvim/init.vim<cr>

" Quick plug-install
noremap <leader>p :PlugInstall<cr>

" Quick Nerdtree toggle
noremap <leader>n :NERDTreeToggle<cr>

" Quick buffer cycle
noremap <C-J> :bprev<cr>
noremap <C-K> :bnext<cr>

" Quick coc mappings
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)
nmap <silent> rn <Plug>(coc-rename)

" Instant floating terminal
let g:floaterm_keymap_toggle = '<F1>'

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
Plug 'tpope/vim-commentary'
Plug 'cespare/vim-toml'
Plug 'dense-analysis/ale'
Plug 'neomake/neomake'

Plug 'neovim/nvim-lspconfig'
Plug 'tjdevries/lsp_extensions.nvim'
Plug 'nvim-lua/completion-nvim'
Plug 'nvim-lua/diagnostic-nvim'

" Floating terminal
Plug 'voldikss/vim-floaterm'

Plug 'fatih/vim-go'

" Nix syntax
Plug 'LnL7/vim-nix'

" Status-line
Plug 'itchyny/lightline.vim'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Completion
Plug 'ctrlpvim/ctrlp.vim'

" Multi-language support
Plug 'sheerun/vim-polyglot'

" Completion
Plug 'neoclide/coc.nvim', {'branch': 'release'}

" Themes
Plug 'joshdick/onedark.vim'

" Friendly neighborhood file explorer
Plug 'preservim/nerdtree'

" Better scrolling
Plug 'psliwka/vim-smoothie'

" Git info while we roll
Plug 'airblade/vim-gitgutter'

" Better commeting support
Plug 'preservim/nerdcommenter'

call plug#end()

" Default theme
colorscheme onedark

" Status line config
let g:lightline = {
      \ 'colorscheme': 'onedark',
      \ }

let g:airline_theme='onedark'
"let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1

" Basic ctrl-p setup (more gory details at
" https://github.com/kien/ctrlp.vim/blob/master/doc/ctrlp.txt)
let g:ctrlp_working_path_mode = 'ra'
let g:ctrlp_map = '<c-p>'

" Rust-analyzer setup:
"
" Set completeopt to have a better completion experience
" :help completeopt
" menuone: popup even when there's only one match
" noinsert: Do not insert text until a selection is made
" noselect: Do not select, force user to select one from the menu
set completeopt=menuone,noinsert,noselect

" Avoid showing extra messages when using completion
set shortmess+=c

" Configure LSP
" https://github.com/neovim/nvim-lspconfig#rust_analyzer
lua <<EOF

-- nvim_lsp object
local nvim_lsp = require'lspconfig'

-- function to attach completion and diagnostics
-- when setting up lsp
local on_attach = function(client)
    require'completion'.on_attach(client)
    require'diagnostic'.on_attach(client)
end

-- Enable rust_analyzer
nvim_lsp.rust_analyzer.setup({ on_attach=on_attach })

EOF

" Go-analyzer setup:

lua <<EOF
require'lspconfig'.gopls.setup{}
EOF

" Some experimental lua stuff
command! Scratch lua require'tools'.makeScratch()


