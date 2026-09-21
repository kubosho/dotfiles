# .files

Personal dotfiles managed by [chezmoi](https://www.chezmoi.io/).

## Setup

1. Install [chezmoi](https://www.chezmoi.io/install/) and [mise](https://mise.jdx.dev/getting-started.html)
2. Initialize with this repository and install the global tools:

```bash
chezmoi init --source ~/src/github.com/kubosho/dotfiles
chezmoi apply
mise bootstrap
```

3. Restart your shell or run `source ~/.zshrc`

## OS support

- macOS
- Windows (WSL)

OS-specific files are managed via `.chezmoiignore` (e.g., WezTerm is excluded on macOS, Ghostty on Windows).
