
-- Some "sensible defaults"

vim.o.termguicolors = true
vim.o.syntax = 'on'
vim.o.errorbells = false
vim.o.smartcase = true
vim.o.showmode = false
vim.bo.swapfile = false
vim.o.backup = false
vim.o.undodir = vim.fn.stdpath('config') .. '/undodir'
vim.o.undofile = true
vim.o.incsearch = true
vim.o.ignorecase = true
vim.o.hidden = true
vim.o.completeopt='menuone,noinsert,noselect'
vim.bo.autoindent = true
vim.bo.smartindent = true
vim.o.tabstop = 2
vim.o.softtabstop = 2
vim.o.shiftwidth = 2
vim.o.expandtab = true
vim.wo.number = true
vim.wo.relativenumber = true
vim.wo.signcolumn = 'yes'
vim.wo.wrap = false

-- Mappings

vim.g.mapleader = ','

local key_mapper = function(mode, key, result)
  vim.api.nvim_set_keymap(
    mode,
    key,
    result,
    {noremap = true, silent = true}
  )
end

-- Don't need no arrow keys

key_mapper('', '<up>', '<nop>')
key_mapper('', '<down>', '<nop>')
key_mapper('', '<left>', '<nop>')
key_mapper('', '<right>', '<nop>')

-- "Escape hatch"

key_mapper('i', 'jk', '<ESC>')
key_mapper('v', 'jk', '<ESC>')

-- My "fast save" and "fast quit"
key_mapper('', '<leader>w', ':w<CR>')
key_mapper('i', '<leader>w', '<C-o>:w<CR>')

key_mapper('', '<leader>q', ':q<CR>')

-- Load config file while editing
key_mapper('', '<leader>cl', ':luafile %<CR>')

-- Telescope mappings
key_mapper('', '<leader>ff', ':Telescope find_files<CR>')
key_mapper('', '<leader>fb', ':Telescope file_browser<CR>')
key_mapper('', '<leader>rg', ':Telescope live_grep<CR>')
key_mapper('', '<leader>bb', ':Telescope buffers<CR>')

-- Git aids
key_mapper('', '<leader>gm', ':GitMessenger<CR>')

-- Some helpers

local vim = vim
local execute = vim.api.nvim_command
local fn = vim.fn

-- ensure that packer is installed
local install_path = fn.stdpath('data')..'/site/pack/packer/opt/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
    execute('!git clone https://github.com/wbthomason/packer.nvim '..install_path)
    execute 'packadd packer.nvim'
end
vim.cmd('packadd packer.nvim')

-- Setting up plugins

local packer = require'packer'
local util = require'packer.util'

---- TODO: revisit this with current path
packer.init({
  package_root = util.join_paths(vim.fn.stdpath('data'), 'site', 'pack')
})

-- List out plugins here

packer.startup(function()
  local use = use

  use 'nvim-treesitter/nvim-treesitter'
  use 'sheerun/vim-polyglot'

  -- these are optional themes but I hear good things about gloombuddy ;)
  -- colorbuddy allows us to run the gloombuddy theme
  use 'tjdevries/colorbuddy.nvim'
  use 'bkegley/gloombuddy'
  use 'neovim/nvim-lspconfig'
  use 'anott03/nvim-lspinstall'

  -- Browser
  use 'nvim-lua/plenary.nvim'
  use 'nvim-telescope/telescope.nvim'
  use 'nvim-telescope/telescope-file-browser.nvim'
  use 'rhysd/git-messenger.vim'

  -- Go support
--  use 'ray-x/go.nvim'
--  use 'ray-x/guihua.lua' -- recommended if need floating window support
  use 'neovim/nvim-lspconfig'
  use 'lewis6991/gitsigns.nvim'
  
end
)

-- Color theme

-- vim.g.colors_name = 'koehler'
vim.cmd [[colorscheme gloombuddy]]

-- TreeSitter
---- TODO

-- LSP config

local lspconfig = require'lspconfig'
--local completion = require'completion'
--local function custom_on_attach(client)
--  print('Attaching to ' .. client.name)
--  completion.on_attach(client)
--end
--local default_config = {
--  on_attach = custom_on_attach,
--}
-- setup language servers here
lspconfig.tsserver.setup({})
lspconfig.gopls.setup({})

-- Telescope
require("telescope").setup {
  extensions = {
    file_browser = {
      -- theme = "ivy",
      -- disables netrw and use telescope-file-browser in its place
      hijack_netrw = true,
      mappings = {
        ["i"] = {
          -- your custom insert mode mappings
        },
        ["n"] = {
          -- your custom normal mode mappings
          --
        },
      },
    },
  },
}
-- To get telescope-file-browser loaded and working with telescope,
-- you need to call load_extension, somewhere after setup function:
require("telescope").load_extension "file_browser"

-- Git integration
require('gitsigns').setup()


