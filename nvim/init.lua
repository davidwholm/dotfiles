-- Install packer
local install_path = vim.fn.stdpath "data" .. "/site/pack/packer/start/packer.nvim"

if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
  vim.fn.execute("!git clone https://github.com/wbthomason/packer.nvim " .. install_path)
end

vim.cmd [[
  augroup Packer
    autocmd!
    autocmd BufWritePost init.lua PackerCompile
  augroup end
]]

local use = require("packer").use
require("packer").startup(function()
  use({ "wbthomason/packer.nvim" })
  use({ "tpope/vim-repeat" })
  use({ "ggandor/lightspeed.nvim" })
  use({
    "nvim-telescope/telescope.nvim",
    requires = {"nvim-lua/plenary.nvim"}
  })
  use({ "nvim-treesitter/nvim-treesitter" })
  use({ "neovim/nvim-lspconfig" })
  use({ "hrsh7th/nvim-cmp" })
  use({ "hrsh7th/cmp-nvim-lsp" })
  use({ "saadparwaiz1/cmp_luasnip" })
  use({ "L3MON4D3/LuaSnip" })
end)

-- Helper functions
local function bind_key(mode, key, cmd, opts)
  vim.api.nvim_set_keymap(mode, key, cmd, opts or { noremap = true, silent = true })
end

-- Telescope
require("telescope").setup()

-- find

bind_key("n", "<leader>ff", [[<cmd>lua require("telescope.builtin").find_files()<CR>]])
bind_key("n", "<leader>fb", [[<cmd>lua require("telescope.builtin").buffers()<CR>]])
bind_key("n", "<leader>fo", [[<cmd>lua require("telescope.builtin").oldfiles()<CR>]])

-- search

bind_key("n", "<leader>sf", [[<cmd>lua require("telescope.builtin").live_grep()<CR>]])
bind_key("n", "<leader>sb", [[<cmd>lua require("telescope.builtin").current_buffer_fuzzy_find()<CR>]])

-- Visuals

-- colorscheme and statusline
vim.opt.termguicolors = true

-- line numbers
vim.opt.number = true

-- don't wrap lines please
vim.opt.wrap = false

-- highlight on yank
vim.cmd [[
  augroup YankHighlight
    autocmd!
    autocmd TextYankPost * silent! lua vim.highlight.on_yank()
  augroup end
]]

-- Options

vim.opt.mouse = "a"
vim.opt.undofile = true

-- Treesitter

require("nvim-treesitter.configs").setup({
  ensure_installed = "maintained",
  highlight = {
    enable = true,
    additional_vim_regex_highlighting = false,
  },
})
