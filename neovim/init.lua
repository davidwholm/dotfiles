-- Install packer
local install_path = vim.fn.stdpath 'data' .. '/site/pack/packer/start/packer.nvim'

if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
  vim.fn.execute('!git clone https://github.com/wbthomason/packer.nvim ' .. install_path)
end

vim.api.nvim_exec(
  [[
  augroup Packer
    autocmd!
    autocmd BufWritePost init.lua PackerCompile
  augroup end
]],
  false
)

local use = require('packer').use
require('packer').startup(function()
  use 'wbthomason/packer.nvim' -- Package manager

  use {
    'Pocco81/Catppuccino.nvim',
    config = function()
      require('catppuccino').setup({
        colorscheme = 'dark_catppuccino'
      })
      if not vim.g.colorscheme_set then
        vim.cmd([[colorscheme catppuccino]])
        vim.g.colorscheme_set = true
      end
    end
  }

  use {
    'ojroques/nvim-hardline',
    config = function()
      require('hardline').setup()
    end
  }


  use { 
    'nvim-telescope/telescope.nvim',
    cmd = 'Telescope',
    module = 'telescope',
    config = function()
      local actions = require('telescope.actions')
      require('telescope').setup({
        defaults = {
          mappings = {
            i = {
              ['<esc>'] = actions.close,
            },
          },
        },
      })
    end,
    requires = { 'nvim-lua/plenary.nvim', 'kyazdani42/nvim-web-devicons' } 
  }

  use {
    'lambdalisue/suda.vim',
    cmd = { 'SudaWrite', 'SudaRead' }
  }

  use { 
    'lewis6991/gitsigns.nvim', 
    config = function()
      require('gitsigns').setup()
    end,
    requires = { 'nvim-lua/plenary.nvim' } 
  }

  use {
    'nvim-treesitter/nvim-treesitter',
    run = ':TSUpdate',
    config = function()
      require('nvim-treesitter.configs').setup({
        ensure_installed = "all", -- one of "all", "maintained" (parsers with maintainers), or a list of languages

        highlight = {
          enable = true,              -- false will disable the whole extension
          additional_vim_regex_highlighting = false,
        },

        incremental_selection = {
          enable = true,

          keymaps = {
            init_selection = 'gnn',
            node_incremental = 'gnn',
            scope_incremental = 'gns',
            node_decremental = 'gnp'
          }
        },

        indent = {
          enable = true
        },

        textobjects = {
          select = {
            enable = true,
            keymaps = {
              ["af"] = "@function.outer",
              ["if"] = "@function.inner",
              ["icd"] = "@conditional.inner",
              ["acd"] = "@conditional.outer",
              ["acm"] = "@comment.outer",
              ["ilp"] = "@loop.inner",
              ["alp"] = "@loop.outer",
              ["ast"] = "@statement.outer",
              ["isc"] = "@scopename.inner",
              ["iB"] = "@block.inner",
              ["aB"] = "@block.outer",
              ["p"] = "@parameter.inner"
            }
          },

          move = {
            enable = true,
            set_jumps = true, -- Whether to set jumps in the jumplist
            goto_next_start = {
              ["gnf"] = "@function.outer",
              ["gnif"] = "@function.inner",
              ["gnp"] = "@parameter.inner",
              ["gnc"] = "@call.outer",
              ["gnic"] = "@call.inner"
              },
            goto_next_end = {
              ["gnF"] = "@function.outer",
              ["gniF"] = "@function.inner",
              ["gnP"] = "@parameter.inner",
              ["gnC"] = "@call.outer",
              ["gniC"] = "@call.inner"
            },
            goto_previous_start = {
              ["gpf"] = "@function.outer",
              ["gpif"] = "@function.inner",
              ["gpp"] = "@parameter.inner",
              ["gpc"] = "@call.outer",
              ["gpic"] = "@call.inner"
            },
            goto_previous_end = {
              ["gpF"] = "@function.outer",
              ["gpiF"] = "@function.inner",
              ["gpP"] = "@parameter.inner",
              ["gpC"] = "@call.outer",
              ["gpiC"] = "@call.inner"
            }
          }
        },
      })
    end,
    requires = {
      'nvim-treesitter/nvim-treesitter-textobjects'
    }
  }

  use {
    'neovim/nvim-lspconfig',
    config = function()
      local nvim_lsp = require('lspconfig')

      local on_attach = function(_, bufnr)
        vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')
        vim.cmd [[ command! Format execute 'lua vim.lsp.buf.formatting()' ]]
      end

      local capabilities = vim.lsp.protocol.make_client_capabilities()
      capabilities = require('cmp_nvim_lsp').update_capabilities(capabilities)

      local servers = { 'clangd', 'racket_langserver' }

      for _, lsp in ipairs(servers) do
        nvim_lsp[lsp].setup {
          on_attach = on_attach,
          capabilities = capabilities,
        }
      end
    end,
    requires = {
      'hrsh7th/nvim-cmp'
    }
  }

  use {
    'hrsh7th/nvim-cmp',
    config = function()
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
        { name = 'buffer' },
        { name = 'path' },
        { name = 'nvim_lsp' },
        { name = 'luasnip' },
      },
    })
    end,
    requires = {
      'hrsh7th/cmp-nvim-lsp',
      'hrsh7th/cmp-buffer',
      'hrsh7th/cmp-path',
      'saadparwaiz1/cmp_luasnip',
      'L3MON4D3/LuaSnip'
    }
  }

  use 'Olical/conjure'

  use 'wlangstroth/vim-racket'
  
  use 'tpope/vim-commentary'
  use 'tpope/vim-repeat'
  use 'tpope/vim-surround'
end)


-- Disable the dreaded wrap
vim.opt.wrap = false

--Set highlight on search
vim.o.hlsearch = false

--Make line numbers default
vim.wo.number = true

--Do not save when switching buffers (note: this is now a default on master)
vim.o.hidden = true

--Enable mouse mode
vim.o.mouse = 'a'

--Enable break indent
vim.o.breakindent = true

--Save undo history
vim.opt.undofile = true

--Case insensitive searching UNLESS /C or capital in search
vim.o.ignorecase = true
vim.o.smartcase = true

--Decrease update time
vim.o.updatetime = 250
vim.wo.signcolumn = 'yes'

-- Indent settings
vim.opt.expandtab = true
vim.opt.shiftwidth = 4
vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.smartindent = true

-- Completion
vim.opt.completeopt = 'menuone,noselect'

-- Highlight on yank
vim.api.nvim_exec(
[[
  augroup YankHighlight
    autocmd!
    autocmd TextYankPost * silent! lua vim.highlight.on_yank()
  augroup end
]], false)

