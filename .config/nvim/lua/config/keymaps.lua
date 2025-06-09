-- Custom keymaps
local keymap = vim.keymap

-- Telescope (file search)
keymap.set("n", "<C-p>", "<cmd>Telescope find_files<cr>", { desc = "Find files" })
keymap.set("n", "<C-S-p>", "<cmd>Telescope commands<cr>", { desc = "Command palette" })
keymap.set("n", "<C-f>", "<cmd>Telescope current_buffer_fuzzy_find<cr>", { desc = "Find in current file" })
keymap.set("n", "<C-S-f>", "<cmd>Telescope live_grep<cr>", { desc = "Find in files" })
keymap.set("n", "<C-S-o>", "<cmd>Telescope buffers<cr>", { desc = "Switch between open files" })

-- nvim-tree (file explorer)
keymap.set("n", "<C-b>", "<cmd>NvimTreeToggle<cr>", { desc = "Toggle file explorer" })

-- Tab navigation (Barbar.nvim)
keymap.set("n", "<A-l>", "<Cmd>BufferNext<CR>", { desc = "Next tab" })
keymap.set("n", "<A-h>", "<Cmd>BufferPrevious<CR>", { desc = "Previous tab" })
keymap.set("n", "<A-t>", "<Cmd>enew<CR>", { desc = "New tab" })
keymap.set("n", "<A-w>", "<Cmd>BufferClose<CR>", { desc = "Close tab" })
keymap.set("n", "<A-1>", "<Cmd>BufferGoto 1<CR>", { desc = "Go to tab 1" })
keymap.set("n", "<A-2>", "<Cmd>BufferGoto 2<CR>", { desc = "Go to tab 2" })
keymap.set("n", "<A-3>", "<Cmd>BufferGoto 3<CR>", { desc = "Go to tab 3" })
keymap.set("n", "<A-4>", "<Cmd>BufferGoto 4<CR>", { desc = "Go to tab 4" })
keymap.set("n", "<A-5>", "<Cmd>BufferGoto 5<CR>", { desc = "Go to tab 5" })
keymap.set("n", "<A-6>", "<Cmd>BufferGoto 6<CR>", { desc = "Go to tab 6" })
keymap.set("n", "<A-7>", "<Cmd>BufferGoto 7<CR>", { desc = "Go to tab 7" })
keymap.set("n", "<A-8>", "<Cmd>BufferGoto 8<CR>", { desc = "Go to tab 8" })
keymap.set("n", "<A-9>", "<Cmd>BufferGoto 9<CR>", { desc = "Go to tab 9" })

-- Comment toggle
keymap.set("n", "<A-/>", "gcc", { desc = "Toggle comment", remap = true })
keymap.set("v", "<A-/>", "gc", { desc = "Toggle comment", remap = true })
