# .files

Personal dotfiles managed by [chezmoi](https://www.chezmoi.io/).

## Setup

1. Install chezmoi
2. Initialize with this repository:

```bash
chezmoi init --source ~/src/github.com/kubosho/dotfiles
chezmoi apply
```

3. Restart your shell or run `source ~/.zshrc`

## OS support

- macOS
- Windows (WSL)

OS-specific files are managed via `.chezmoiignore` (e.g., WezTerm is excluded on macOS, Ghostty on Windows).
