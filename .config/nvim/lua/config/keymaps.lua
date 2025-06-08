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

-- VSCode-like tab navigation
keymap.set("n", "<C-Tab>", ":bnext<CR>", { desc = "Next tab" })
keymap.set("n", "<C-S-Tab>", ":bprevious<CR>", { desc = "Previous tab" })

-- Comment toggle (VSCode-like)
keymap.set("n", "<C-/>", "gcc", { desc = "Toggle comment", remap = true })
keymap.set("v", "<C-/>", "gc", { desc = "Toggle comment", remap = true })
