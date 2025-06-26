------------------------------------------------
-- Import plugins
--------------------------------------------------

if not vim.g.vscode then
  require("config.lazy")
end

------------------------------------------------
-- Load keymaps
--------------------------------------------------

if not vim.g.vscode then
  require("config.keymaps")
end

------------------------------------------------
-- Editor settings
--------------------------------------------------

if not vim.g.vscode then
  -- Show line numbers
  vim.opt.number = true
  vim.opt.relativenumber = false

  -- Force line numbers in all windows except specific filetypes
  vim.api.nvim_create_autocmd({ "BufEnter", "WinEnter" }, {
    pattern = "*",
    callback = function()
      local excluded_filetypes = { "NvimTree", "neo-tree", "alpha", "dashboard", "startify" }
      if not vim.tbl_contains(excluded_filetypes, vim.bo.filetype) then
        vim.opt_local.number = true
      end
    end,
  })

  -- Show invisible characters
  vim.opt.list = true
  vim.opt.listchars = { tab = '▸ ', trail = '-', nbsp = ' ' }

  -- Enable true color support
  vim.opt.termguicolors = true

  -- Background transparency
  vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
  vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })
end

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

  -- For WSL
elseif vim.env.WINDOWS_PATH then
  vim.g.clipboard = {
    name = 'WSL Clipboard',
    copy = {
      ['+'] = vim.env.WINDOWS_PATH .. '/System32/clip.exe',
      ['*'] = vim.env.WINDOWS_PATH .. '/System32/clip.exe',
    },
    paste = {
      ['+'] = vim.env.WINDOWS_PATH ..
          '/System32/powershell.exe -c [Console]::Out.Write($(Get-Clipboard -Raw).tostring().replace("`r", ""))',
      ['*'] = vim.env.WINDOWS_PATH ..
          '/System32/powershell.exe -c [Console]::Out.Write($(Get-Clipboard -Raw).tostring().replace("`r", ""))',
    },
    cache_enabled = 0,
  }
end
