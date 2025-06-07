# .files

Personal dotfiles management tool that automatically sets up development environment configurations across multiple platforms.

## Overview

### Features

- Automatic symlink creation for dotfiles to home directory
- Cross-platform support (macOS, Windows, WSL)
- Cursor editor settings synchronization
- Selective file exclusion with `.dotfilesignore`

## Usage

### Prerequisites

- Go (for running the setup script)

### Setup

1. Clone this repository
2. Navigate to the repository directory
3. Run the setup script:

```bash
go run setup.go
```

**Note**: The script will overwrite existing dotfiles in your home directory with symlinks.

## Configuration

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
