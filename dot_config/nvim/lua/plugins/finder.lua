return {
  "nvim-telescope/telescope.nvim",
  tag = "0.1.5",
  dependencies = {
    "nvim-lua/plenary.nvim",
    {
      "nvim-telescope/telescope-fzf-native.nvim",
      build = "make",
      cond = function()
        return vim.fn.executable "make" == 1
      end,
    },
  },
  config = function()
    require("telescope").setup {
      defaults = {
        prompt_prefix = " ",
        selection_caret = " ",
        path_display = { "truncate" },
        winblend = 0,
        border = {},
        borderchars = { "─", "│", "─", "│", "╭", "╮", "╯", "╰" },
        color_devicons = true,
        use_less = true,
        set_env = { ["COLORTERM"] = "truecolor" },
        file_previewer = require("telescope.previewers").vim_buffer_cat.new,
        grep_previewer = require("telescope.previewers").vim_buffer_vimgrep.new,
        qflist_previewer = require("telescope.previewers").vim_buffer_qflist.new,
        buffer_previewer_maker = require("telescope.previewers").buffer_previewer_maker,
        mappings = {
          i = {
            ["<C-n>"] = require("telescope.actions").cycle_history_next,
            ["<C-p>"] = require("telescope.actions").cycle_history_prev,
            ["<C-j>"] = require("telescope.actions").move_selection_next,
            ["<C-k>"] = require("telescope.actions").move_selection_previous,
            ["<C-c>"] = require("telescope.actions").close,
            ["<Down>"] = require("telescope.actions").move_selection_next,
            ["<Up>"] = require("telescope.actions").move_selection_previous,
            ["<CR>"] = require("telescope.actions").select_default,
            ["<C-x>"] = require("telescope.actions").select_horizontal,
            ["<C-v>"] = require("telescope.actions").select_vertical,
            ["<C-t>"] = require("telescope.actions").select_tab,
            ["<C-u>"] = require("telescope.actions").preview_scrolling_up,
            ["<C-d>"] = require("telescope.actions").preview_scrolling_down,
            ["<PageUp>"] = require("telescope.actions").results_scrolling_up,
            ["<PageDown>"] = require("telescope.actions").results_scrolling_down,
            ["<Tab>"] = require("telescope.actions").toggle_selection + require("telescope.actions").move_selection_worse,
            ["<S-Tab>"] = require("telescope.actions").toggle_selection + require("telescope.actions").move_selection_better,
            ["<C-q>"] = require("telescope.actions").send_to_qflist + require("telescope.actions").open_qflist,
            ["<M-q>"] = require("telescope.actions").send_selected_to_qflist + require("telescope.actions").open_qflist,
            ["<C-l>"] = require("telescope.actions").complete_tag,
            ["<C-_>"] = require("telescope.actions").which_key,
          },
          n = {
            ["<esc>"] = require("telescope.actions").close,
            ["<CR>"] = require("telescope.actions").select_default,
            ["<C-x>"] = require("telescope.actions").select_horizontal,
            ["<C-v>"] = require("telescope.actions").select_vertical,
            ["<C-t>"] = require("telescope.actions").select_tab,
            ["<Tab>"] = require("telescope.actions").toggle_selection + require("telescope.actions").move_selection_worse,
            ["<S-Tab>"] = require("telescope.actions").toggle_selection + require("telescope.actions").move_selection_better,
            ["<C-q>"] = require("telescope.actions").send_to_qflist + require("telescope.actions").open_qflist,
            ["<M-q>"] = require("telescope.actions").send_selected_to_qflist + require("telescope.actions").open_qflist,
            ["j"] = require("telescope.actions").move_selection_next,
            ["k"] = require("telescope.actions").move_selection_previous,
            ["H"] = require("telescope.actions").move_to_top,
            ["M"] = require("telescope.actions").move_to_middle,
            ["L"] = require("telescope.actions").move_to_bottom,
            ["<Down>"] = require("telescope.actions").move_selection_next,
            ["<Up>"] = require("telescope.actions").move_selection_previous,
            ["gg"] = require("telescope.actions").move_to_top,
            ["G"] = require("telescope.actions").move_to_bottom,
            ["<C-u>"] = require("telescope.actions").preview_scrolling_up,
            ["<C-d>"] = require("telescope.actions").preview_scrolling_down,
            ["<PageUp>"] = require("telescope.actions").results_scrolling_up,
            ["<PageDown>"] = require("telescope.actions").results_scrolling_down,
            ["?"] = require("telescope.actions").which_key,
          },
        },
      },
      pickers = {
        find_files = {
          theme = "dropdown",
          previewer = false,
          -- fd respects .gitignore by default
          hidden = true,
        },
      },
      extensions = {
        fzf = {
          fuzzy = true,
          override_generic_sorter = true,
          override_file_sorter = true,
          case_mode = "smart_case",
        },
      },
    }

    -- Load fzf extension if available
    pcall(require("telescope").load_extension, "fzf")
  end,
}
