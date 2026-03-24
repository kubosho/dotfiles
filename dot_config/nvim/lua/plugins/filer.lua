return {
  "nvim-tree/nvim-tree.lua",
  version = "*",
  lazy = false,
  dependencies = {
    "nvim-tree/nvim-web-devicons",
  },
  config = function()
    require("nvim-tree").setup {
      -- disable netrw at the very start of your init.lua
      disable_netrw = true,
      hijack_netrw = true,

      -- opens the tree when changing/opening a new tab if the tree was previously open
      open_on_tab = false,

      -- hijacks new directory buffers when they are opened
      hijack_directories = {
        enable = true,
        auto_open = true,
      },

      -- will enable file highlight for git attributes using NvimTreeGitFileHL
      git = {
        enable = true,
        ignore = true,
        timeout = 400,
      },

      -- automatically update nvim-tree to show current file location
      update_focused_file = {
        enable = true,
        update_cwd = false,
        ignore_list = {},
      },

      view = {
        -- width of the window, can be either a number (columns) or a string in `%`
        width = 30,
        -- side of the tree, can be one of 'left' | 'right'
        side = "left",
      },

      renderer = {
        -- compact folders that only contain a single folder into one node in the file tree
        group_empty = false,
        highlight_git = false,
        full_name = false,
        highlight_opened_files = "none",
        root_folder_modifier = ":~",
        indent_markers = {
          enable = false,
          icons = {
            corner = "└ ",
            edge = "│ ",
            item = "│ ",
            none = "  ",
          },
        },
        icons = {
          webdev_colors = true,
          git_placement = "before",
          padding = " ",
          symlink_arrow = " ➛ ",
          show = {
            file = true,
            folder = true,
            folder_arrow = true,
            git = true,
          },
          glyphs = {
            default = "",
            symlink = "",
            folder = {
              arrow_closed = "",
              arrow_open = "",
              default = "",
              open = "",
              empty = "",
              empty_open = "",
              symlink = "",
              symlink_open = "",
            },
            git = {
              unstaged = "✗",
              staged = "✓",
              unmerged = "",
              renamed = "➜",
              untracked = "★",
              deleted = "",
              ignored = "◌",
            },
          },
        },
      },
    }
  end,
}
