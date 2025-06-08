------------------------------------------------
-- Import plugins
--------------------------------------------------

require("config.lazy")

------------------------------------------------
-- Load keymaps
--------------------------------------------------

require("config.keymaps")

------------------------------------------------
-- Editor settings
--------------------------------------------------

-- Show line numbers
vim.opt.number = true
vim.opt.relativenumber = false

-- Enable true color support
vim.opt.termguicolors = true

-- Background transparency
vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })

------------------------------------------------
-- Clipboard
--------------------------------------------------

vim.opt.clipboard = "unnamed"

-- For Windows
if vim.fn.has('win32') == 1 then
  vim.g.clipboard = {
    name = 'myClipboard',
    copy = {
      ['+'] = 'win32yank.exe -i',
      ['*'] = 'win32yank.exe -i',
    },
    paste = {
      ['+'] = 'win32yank.exe -o',
      ['*'] = 'win32yank.exe -o',
    },
    cache_enabled = 1,
  }
end
