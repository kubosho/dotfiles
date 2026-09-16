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

## Codex configuration

`dot_codex/modify_private_config.toml` merges settings into the existing
`~/.codex/config.toml` using a chezmoi modify template.

- `.chezmoitemplates/codex-config.toml.tmpl` contains shared settings.
- `dot_codex/private_config.local.toml` optionally contains explicit settings for
  this machine. It is ignored by Git and is not deployed as a separate file.
- Existing values not specified in either source are preserved, including app
  versions, notification commands, project trust, hook state, and UI state.

Local settings override shared settings, which override existing values. Nested
tables are merged. Keep app-maintained values out of the local file so that old
snapshots do not overwrite app updates. With no local file, the shared settings
select `workspace-with-dev-tools` as the default permissions profile. When a local
file exists, it can set `default_permissions` explicitly, as before.

Removing a setting from a source stops managing it but does not delete it from
`~/.codex/config.toml`. Delete it there as well if it should no longer exist.
When values change, TOML is serialized again, so comments and formatting may
change. When values are unchanged, the original file is returned verbatim.

Preview changes with `chezmoi diff ~/.codex/config.toml`. Test the merge and apply
behavior with Python 3.11 or later and chezmoi installed:

```bash
python3 -m unittest discover -s tests -v
```
