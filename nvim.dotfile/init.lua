local opt = vim.opt
local keymap = vim.keymap

opt.hidden = true

-- allow external vimrc
opt.exrc = true

local Plug = vim.fn['plug#']

-- Setup plugins

vim.call('plug#begin')

-- Defaults
Plug 'tpope/vim-sensible'

-- Basic usage
Plug 'tpope/vim-vinegar'

-- Navigation
Plug 'christoomey/vim-tmux-navigator'
Plug('junegunn/fzf', {['do'] = vim.fn['fzf#install']})
Plug 'junegunn/fzf.vim'

-- Styling
Plug('lifepillar/vim-solarized8', { ['branch'] = 'neovim' })
Plug 'nvim-lualine/lualine.nvim'

-- General Usability
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'roxma/vim-tmux-clipboard'
Plug 'jenterkin/vim-autosource'

-- Development
Plug 'w0rp/ale'
Plug 'scrooloose/nerdcommenter'
Plug 'sheerun/vim-polyglot'
Plug 'lepture/vim-jinja'
Plug 'Joorem/vim-haproxy'

-- Snippets
Plug 'honza/vim-snippets'
Plug 'dcampos/nvim-snippy'

-- Language Specific
Plug('davidgranstrom/scnvim', { ['do'] = vim.fn['scnvim#install'], ['for']  ='supercollider' })
Plug('davidgranstrom/scnvim-tmux', { ['for']  ='supercollider' })

vim.call('plug#end')

opt.number = true
opt.ruler = true
opt.shortmess:append({ I = true })

opt.updatetime = 300

-- Turns off swap files and backups
opt.backup = false
opt.writebackup = false
opt.swapfile = false

-- Turn off folding
opt.foldenable = false

-- Turn off mode display
opt.showmode = false

-- Formatting options
opt.expandtab = true
opt.tabstop = 2
opt.softtabstop = 2
opt.shiftwidth = 2

opt.spelllang = "en_gb"

opt.incsearch = true
-- This unsets the "last search pattern" register by hitting return
keymap.set("n", "<CR>", ":noh<CR><CR>", {desc = 'Clear last search pattern'})

-- Colorscheme settings
opt.background = "dark"
vim.cmd.colorscheme('solarized8')

-- Disable arrow keys
keymap.set('', '<up>', '<nop>')
keymap.set('', '<down>', '<nop>')
keymap.set('', '<left>', '<nop>')
keymap.set('', '<right>', '<nop>')
keymap.set('i', '<up>', '<nop>')
keymap.set('i', '<down>', '<nop>')
keymap.set('i', '<left>', '<nop>')
keymap.set('i', '<right>', '<nop>')

vim.g.mapleader = ','

vim.api.nvim_create_user_command('Cheats', ':help cheats.txt', {})

-- Copy to clipboard
keymap.set('n', '<leader>y', '"+y')
keymap.set('v', '<leader>y', '"+y')
keymap.set('n', '<leader>Y', '"+yg_')
keymap.set('n', '<leader>yy', '"+yy')

-- Paste from clipboard
keymap.set('n', '<leader>p', '"+p')
keymap.set('n', '<leader>P', '"+P')
keymap.set('v', '<leader>p', '"+p')
keymap.set('v', '<leader>P', '"+P')

opt.splitbelow = true
opt.switchbuf = 'vsplit'
opt.splitright = true

-- File nav settings
keymap.set('n', '<leader>f', ':Lexplore<CR>')
keymap.set('n', '<leader>r', ':Files<CR>')
keymap.set('n', '<leader>b', ':Buffers<CR>')
keymap.set('n', '<leader>g', ':Ag<Space>')

if vim.fn.executable('ag') == 1 then
  vim.g.ackprg = 'ag --vimgrep'
end

-- ALE settings
vim.g.ale_linters = {
  ['terraform'] = { 'tflint' },
  ['yaml'] = { 'yamllint' },
  ['python'] = { 'flake8' },
  ['c'] = { 'clang' },
}
vim.g.ale_python_flake8_options = '--max-line-length 99'

vim.g.ale_fix_on_save = 1
vim.g.ale_fixers = {
  ['*'] = { 'remove_trailing_lines', 'trim_whitespace' },
  ['terraform'] = { 'terraform' },
  ['python'] = { 'black' },
  ['c'] = { 'clang-format' },
  ['javascript'] = { 'eslint' },
  ['typescript'] = { 'prettier', 'eslint' },
  ['typescriptreact'] = { 'prettier', 'eslint' },
  ['rust'] = { 'rustfmt' },
}
vim.g.ale_python_black_options = '--line-length 99'

require('lualine').setup {
  options = {
    icons_enabled = false,
  },
  sections = {
    lualine_a = {'mode'},
    lualine_b = {'branch', 'diff', 'diagnostics'},
    lualine_c = {'filename'},
    lualine_x = {'encoding', 'fileformat', 'filetype'},
    lualine_y = {'progress'},
    lualine_z = {'location'}
  },
  inactive_sections = {
    lualine_a = {},
    lualine_b = {},
    lualine_c = {'filename'},
    lualine_x = {'location'},
    lualine_y = {},
    lualine_z = {}
  },
}

local snippy = require('snippy')

keymap.set('i', '<Tab>', snippy.mapping.expand_or_advance('<Tab>'))
keymap.set('s', '<Tab>', snippy.mapping.next('<Tab>'))
keymap.set({ 'i', 's' }, '<S-Tab>', snippy.mapping.previous('<S-Tab>'))
keymap.set('x', '<Tab>', snippy.mapping.cut_text, { remap = true })
keymap.set('n', 'g<Tab>', snippy.mapping.cut_text, { remap = true })


local scnvim = require 'scnvim'
local map = scnvim.map
local map_expr = scnvim.map_expr

scnvim.setup({
  keymaps = {
    ['<leader>e'] = map('editor.send_line', {'i', 'n'}),
    ['<leader>a'] = {
      map('editor.send_block', {'i', 'n'}),
      map('editor.send_selection', 'x'),
    },
    ['<C-CR>'] = {
      map('editor.send_block', {'i', 'n'}),
      map('editor.send_selection', 'x'),
    },
    --['<C-k>'] = map('signature.show', {'n', 'i'}),
    ['<leader>hh'] = map('sclang.hard_stop', {'n', 'x', 'i'}),
    ['<leader>st'] = map('sclang.start'),
    ['<leader>sk'] = map('sclang.recompile'),
    --['<F1>'] = map_expr('s.boot'),
    --['<leader>m'] = map_expr('s.meter'),
  },
  editor = {
    highlight = {
      color = 'IncSearch',
    },
  },
  postwin = {
    float = {
      enabled = true,
    },
  },
  sclang = {
    cmd = '/Applications/SuperCollider.app/Contents/MacOS/sclang'
  },
})

vim.api.nvim_create_user_command('SCNvimToggle',
  function()
    postwin = require('scnvim.postwin')
    postwin.toggle()
  end,
  {}
)

scnvim.load_extension 'tmux'

--opt.secure = true
