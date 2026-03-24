# AGENTS.md

This file provides guidance to AI coding agents when working with code in this repository.

## Repository Overview

Dotfiles repository managed by [chezmoi](https://www.chezmoi.io/). Files use `dot_` prefix (e.g., `dot_gitconfig` → `~/.gitconfig`).

## Commands

```bash
chezmoi apply          # Apply dotfiles to home directory
chezmoi diff           # Preview changes before applying
chezmoi add ~/.file    # Add a file to be managed
chezmoi edit ~/.file   # Edit a managed file
```

## Structure

- `dot_config/` → `~/.config/` (jj/, starship.toml)
- `dot_claude/` → `~/.claude/`
- `dot_agents/` → `~/.agents/`
- Shell: `dot_zshenv`, `dot_zshrc`, `dot_commonenv`
- Git: `dot_gitconfig`

## Environment

- macOS and WSL supported
- devbox for global tool management
- Starship prompt with Jujutsu VCS integration
