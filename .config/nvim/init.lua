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
