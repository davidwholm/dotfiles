-- vim: foldmethod=marker

-- Packer {{{
local install_path = vim.fn.stdpath 'data' .. '/site/pack/packer/start/packer.nvim'

if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
  vim.fn.execute('!git clone https://github.com/wbthomason/packer.nvim ' .. install_path)
end
-- }}}

-- Plugins {{{
local use = require('packer').use
require('packer').startup(function()
  use({'wbthomason/packer.nvim'})
  use({'ChristianChiarulli/nvcode-color-schemes.vim'})
  use({
    'nvim-telescope/telescope.nvim',
    requires = {
      'nvim-lua/plenary.nvim', 
      'kyazdani42/nvim-web-devicons'
    }
  })
  use({'lambdalisue/suda.vim'})
  use({
    'nvim-treesitter/nvim-treesitter',
    run = ':TSUpdate'
  })
  use({'neovim/nvim-lspconfig'})
  use ({'kristijanhusak/orgmode.nvim'})
  use({
    'hrsh7th/nvim-cmp',
     requires = {
      'hrsh7th/cmp-nvim-lsp',
      'hrsh7th/cmp-path',
      'saadparwaiz1/cmp_luasnip',
      'L3MON4D3/LuaSnip'
    }
  })
end)
-- }}}

-- Visuals {{{
vim.opt.termguicolors = true
vim.opt.number = true
vim.opt.guifont = "JetBrainsMono Nerd Font:h18"
vim.cmd([[colorscheme nvcode]])
-- }}}

-- Org-mode {{{
local parser_config = require "nvim-treesitter.parsers".get_parser_configs()
parser_config.org = {
  install_info = {
    url = 'https://github.com/milisims/tree-sitter-org',
    revision = 'main',
    files = {'src/parser.c', 'src/scanner.cc'},
  },
  filetype = 'org',
}
require('orgmode').setup({
  org_agenda_files = { '~/Nextcloud/org' },
  org_default_notes_file = '~/Nextcloud/org/refile.org'
})
vim.api.nvim_set_keymap(
  'n',
  '<leader>os',
  [[<cmd>lua require('telescope.builtin').live_grep({search_dirs={'$HOME/Nextcloud/org'}})<cr>]],
  { noremap = true, silent = true }
)

vim.api.nvim_set_keymap(
  'n',
  '<leader>of',
  [[<cmd>lua require('telescope.builtin').find_files({search_dirs={'$HOME/Nextcloud/org'}})<cr>]],
  { noremap = true, silent = true }
)
-- }}}

-- Telescope {{{
require('telescope').setup()
-- }}}

-- Treesitter {{{
require'nvim-treesitter.configs'.setup {
  ensure_installed = "maintained", "org",
  highlight = {
    enable = true,
    additional_vim_regex_highlighting = {'org'}
  },
  indent = {
    enable = true
  },
}
-- }}}

-- Lsp {{{
local nvim_lsp = require('lspconfig')

local on_attach = function(_, bufnr)
  vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')
  vim.cmd [[ command! Format execute 'lua vim.lsp.buf.formatting()' ]]
end

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').update_capabilities(capabilities)

local servers = { }

for _, lsp in ipairs(servers) do
  nvim_lsp[lsp].setup {
    on_attach = on_attach,
    capabilities = capabilities,
  }
end
-- }}}

-- Completion {{{
local luasnip = require('luasnip')
local cmp = require('cmp')
cmp.setup({
  snippet = {
    expand = function(args)
      require('luasnip').lsp_expand(args.body)
    end,
  },
  mapping = {
    ['<C-p>'] = cmp.mapping.select_prev_item(),
    ['<C-n>'] = cmp.mapping.select_next_item(),
    ['<C-d>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<C-e>'] = cmp.mapping.close(),
    ['<CR>'] = cmp.mapping.confirm {
      behavior = cmp.ConfirmBehavior.Replace,
      select = true,
    },
    ['<Tab>'] = function(fallback)
      if cmp.visible() then
	cmp.select_next_item()
      elseif luasnip.expand_or_jumpable() then
	luasnip.expand_or_jump()
      else
	fallback()
      end
    end,
    ['<S-Tab>'] = function(fallback)
      if cmp.visible() then
	cmp.select_prev_item()
      elseif luasnip.jumpable(-1) then
	luasnip.jump(-1)
      else
	fallback()
      end
    end,
  },
  sources = {
    { name = 'path' },
    { name = 'nvim_lsp' },
    { name = 'luasnip' },
    { name = 'orgmode' }
  },
})
-- }}}
