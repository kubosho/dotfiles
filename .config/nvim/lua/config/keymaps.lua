-- Custom keymaps
local keymap = vim.keymap

-- File operations
keymap.set("n", "<C-s>", ":w<CR>", { desc = "Save file" })
keymap.set("i", "<C-s>", "<Esc>:w<CR>a", { desc = "Save file" })

-- Telescope (VSCode-like file search)
keymap.set("n", "<C-p>", "<cmd>Telescope find_files<cr>", { desc = "Find files" })
keymap.set("n", "<C-S-p>", "<cmd>Telescope commands<cr>", { desc = "Command palette" })
keymap.set("n", "<C-f>", "<cmd>Telescope current_buffer_fuzzy_find<cr>", { desc = "Find in current file" })
keymap.set("n", "<C-S-f>", "<cmd>Telescope live_grep<cr>", { desc = "Find in files" })
keymap.set("n", "<C-S-o>", "<cmd>Telescope buffers<cr>", { desc = "Switch between open files" })

-- nvim-tree (VSCode-like file explorer)
keymap.set("n", "<C-b>", "<cmd>NvimTreeToggle<cr>", { desc = "Toggle file explorer" })
keymap.set("n", "<C-S-e>", "<cmd>NvimTreeFindFile<cr>", { desc = "Reveal file in explorer" })

-- VSCode-like tab navigation (Barbar.nvim)
keymap.set("n", "<C-Tab>", "<Cmd>BufferNext<CR>", { desc = "Next tab" })
keymap.set("n", "<C-S-Tab>", "<Cmd>BufferPrevious<CR>", { desc = "Previous tab" })
keymap.set("n", "<C-w>", "<Cmd>BufferClose<CR>", { desc = "Close tab" })
keymap.set("n", "<C-1>", "<Cmd>BufferGoto 1<CR>", { desc = "Go to tab 1" })
keymap.set("n", "<C-2>", "<Cmd>BufferGoto 2<CR>", { desc = "Go to tab 2" })
keymap.set("n", "<C-3>", "<Cmd>BufferGoto 3<CR>", { desc = "Go to tab 3" })
keymap.set("n", "<C-4>", "<Cmd>BufferGoto 4<CR>", { desc = "Go to tab 4" })
keymap.set("n", "<C-5>", "<Cmd>BufferGoto 5<CR>", { desc = "Go to tab 5" })
keymap.set("n", "<C-6>", "<Cmd>BufferGoto 6<CR>", { desc = "Go to tab 6" })
keymap.set("n", "<C-7>", "<Cmd>BufferGoto 7<CR>", { desc = "Go to tab 7" })
keymap.set("n", "<C-8>", "<Cmd>BufferGoto 8<CR>", { desc = "Go to tab 8" })
keymap.set("n", "<C-9>", "<Cmd>BufferGoto 9<CR>", { desc = "Go to tab 9" })

-- Comment toggle (VSCode-like)
keymap.set("n", "<C-/>", "gcc", { desc = "Toggle comment", remap = true })
keymap.set("v", "<C-/>", "gc", { desc = "Toggle comment", remap = true })
