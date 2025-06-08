# Neovim Configuration

This Neovim configuration provides a VSCode-like experience with modern plugin management using lazy.nvim.

## Structure

```text
.config/nvim/
├── init.lua                 # Main configuration entry point
├── lua/
│   ├── config/
│   │   ├── lazy.lua        # lazy.nvim bootstrap and setup
│   │   └── keymaps.lua     # VSCode-like key mappings
│   └── plugins/
│       ├── comment.lua     # Comment toggle configuration
│       ├── completion.lua  # Autocompletion configuration (nvim-cmp)
│       ├── filer.lua       # File explorer configuration (nvim-tree)
│       ├── finder.lua      # Fuzzy finder configuration (telescope)
│       ├── lsp.lua         # LSP configuration with Mason
│       └── theme.lua       # Color theme configuration
└── README.md               # This file
```

## Theme

Use [Tokyo Night](https://github.com/folke/tokyonight.nvim). Tokyo Night is Light/dark color scheme with background transparency support.

- **Light Theme**: Tokyo Night Day (default)
- **Background**: Transparent for terminal integration
- **Features**: TrueColor support, customizable styles

## Installed Plugins

### nvim-tree

File explorer similar to VSCode's sidebar.

- **Toggle**: `Ctrl+B`
- **Reveal current file**: `Ctrl+Shift+E`

### telescope.nvim

Fuzzy finder for files, text search, and more.

- **Find files**: `Ctrl+P`
- **Command palette**: `Ctrl+Shift+P`
- **Search in current file**: `Ctrl+F`
- **Search in all files**: `Ctrl+Shift+F`
- **Switch between open files**: `Ctrl+Shift+O`

### LSP (Language Server Protocol)

Provides IDE-like features with automatic language server installation via Mason.

#### Mason Commands

- `:Mason` - Open Mason UI to manage language servers
- `:MasonInstall <server>` - Install a specific language server
- `:MasonUninstall <server>` - Uninstall a language server
- `:MasonUpdate` - Update all installed servers

#### LSP Key Mappings

- `gd` - Go to definition
- `gD` - Go to declaration
- `gi` - Go to implementation
- `gr` - Find references
- `K` - Show hover documentation
- `<C-k>` - Show signature help
- `<leader>rn` - Rename symbol
- `<leader>ca` - Code actions
- `<leader>f` - Format file
- `[d` - Previous diagnostic
- `]d` - Next diagnostic
- `<leader>e` - Show diagnostic float
- `<leader>q` - Show diagnostics list

#### Pre-configured Language Servers

The following language servers are automatically installed:

- **lua_ls** - Lua
- **ts_ls** - TypeScript/JavaScript
- **html** - HTML
- **cssls** - CSS
- **jsonls** - JSON
- **yamlls** - YAML
- **bashls** - Bash/Shell
- **pyright** - Python
- **rust_analyzer** - Rust
- **gopls** - Go

### nvim-cmp

Autocompletion plugin that provides intelligent code completion.

#### Insert Mode Completion

- `<C-Space>` - Trigger completion menu
- `<CR>` - Confirm completion
- Default navigation uses arrow keys

#### Command Line Completion

- **Search mode** (`/` or `?`): Autocompletes from current buffer
- **Command mode** (`:`): Autocompletes Vim commands
- Uses standard cmdline mappings for navigation

### Comment.nvim

Smart commenting plugin with VSCode-like keybindings.

- `Ctrl+/` - Toggle comment for current line or selection
- `gcc` - Toggle line comment
- `gbc` - Toggle block comment
- `gc{motion}` - Comment with motion (e.g., `gcap` for paragraph)
- `gco` - Add comment below current line
- `gcO` - Add comment above current line
- `gcA` - Add comment at end of line

## Custom Key Mappings

### Leader Key Configuration

The leader key is configured in `lua/config/lazy.lua`:

- **Main Leader Key**: `<Space>` (Space bar)
- **Local Leader Key**: `\` (Backslash)

When you see `<leader>` in key mappings, it refers to the Space key. For example:

- `<leader>e` means `<Space>e`
- `<leader>rn` means `<Space>rn`

### File Operations

- `Ctrl+S` - Save file (works in normal and insert mode)

### File Navigation

- `Ctrl+P` - Find files (Telescope)
- `Ctrl+Shift+P` - Command palette (Telescope)
- `Ctrl+F` - Search in current file (Telescope)
- `Ctrl+Shift+F` - Search in all files (Telescope)
- `Ctrl+Shift+O` - Switch between open files (Telescope)
- `Ctrl+B` - Toggle file explorer (nvim-tree)
- `Ctrl+Shift+E` - Reveal current file in explorer (nvim-tree)

### Tab Navigation

- `Ctrl+Tab` - Next buffer
- `Ctrl+Shift+Tab` - Previous buffer

### Commenting

- `Ctrl+/` - Toggle comment (works with selection)

## Adding New Plugins

To add a new plugin, create a new file in `lua/plugins/` directory:

```lua
-- lua/plugins/plugin-name.lua
return {
  "author/plugin-name",
  config = function()
    require("plugin-name").setup({
      -- configuration options
    })
  end,
}
```

The plugin will be automatically loaded by lazy.nvim on the next Neovim startup.

## Requirements

- Neovim 0.8.0 or higher
- Git (for plugin installation)
- A Nerd Font (for icons in nvim-tree)
- ripgrep (for telescope live_grep)
- fd (optional, for better file finding performance)
