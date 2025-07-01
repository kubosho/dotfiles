# .files

Personal dotfiles management tool that automatically sets up development environment configurations across multiple platforms.

## Overview

### Features

- Automatic symlink creation for dotfiles to home directory
- **File/directory copying for Claude Code compatibility** - Copies specific files/directories instead of creating symlinks to resolve Claude Code's symlink issues
- Cross-platform support (macOS, Windows, WSL)
- Cursor editor settings synchronization
- Selective file exclusion with `.dotfilesignore`
- Dry-run mode to preview operations
- Interactive confirmation for existing files

## Usage

### Prerequisites

- Go (for running the setup script)

### Setup

1. Clone this repository
2. Navigate to the repository directory
3. Run the setup script:

```bash
# Normal execution
go run setup.go

# Dry-run mode (preview operations without making changes)
go run setup.go --dry-run
```

### Behavior

- **Symlinks**: Most dotfiles will be symlinked to your home directory
- **File copying**: Specific files/directories (like `.claude`) will be copied instead of symlinked to ensure Claude Code compatibility
- **Existing files**:
  - Symlinks are automatically replaced
  - Regular files/directories prompt for confirmation before removal
  - You can skip files by choosing "No" when prompted

## Configuration

### Copy targets

Files and directories defined in `COPY_TARGETS` (in `setup.go`) will be copied instead of symlinked. Currently configured for:

- `.claude` directory and its contents
- `.claude/commands` directory

To modify copy targets, edit the `COPY_TARGETS` variable in `setup.go`.

### Excluding files

Use `.dotfilesignore` to exclude specific files from symlink creation. Supports directory patterns and wildcards.

### Cursor settings sync

- macOS: Automatically syncs to `~/Library/Application Support/Cursor/User/settings.json`
- WSL: Copies to Windows home directory (requires `$WINDOWS_HOME` environment variable)

## System requirements

- Windows
- Windows Subsystem for Linux
- macOS

## Troubleshooting

### WSL Setup

For WSL users, set the `WINDOWS_HOME` environment variable:

```bash
export WINDOWS_HOME="/mnt/c/Users/YourUsername"
```

### Existing Files

The setup script will remove existing files before creating symlinks. Back up important configurations before running.
