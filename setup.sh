#!/bin/bash
set -e

# Color output
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m'

echo -e "${GREEN}Setting up dotfiles...${NC}"

# Get current directory
DOTFILES_DIR="$(cd "$(dirname "$0")" && pwd)"
HOME_DIR="$HOME"

# Read .dotfilesignore patterns into array
declare -a ignore_patterns=(".git")
if [ -f ".dotfilesignore" ]; then
    while IFS= read -r line || [ -n "$line" ]; do
        # Skip empty lines and comments
        [[ -z "$line" || "$line" =~ ^#.*$ || "$line" =~ ^!.*$ ]] && continue
        ignore_patterns+=("$line")
    done < ".dotfilesignore"
fi

# Pattern matching function
should_ignore() {
    local file="$1"
    local basename=$(basename "$file")
    
    for pattern in "${ignore_patterns[@]}"; do
        # Directory pattern
        if [[ "$pattern" == */ ]]; then
            pattern="${pattern%/}"
            [[ "$file" == *"$pattern"* ]] && return 0
        fi
        # Wildcard pattern
        if [[ "$basename" == $pattern ]]; then
            return 0
        fi
    done
    return 1
}

# Create symlinks for dotfiles
echo -e "${YELLOW}Creating symlinks for dotfiles...${NC}"
for file in "$DOTFILES_DIR"/.*; do
    basename=$(basename "$file")
    
    # Skip . and ..
    [[ "$basename" == "." || "$basename" == ".." ]] && continue
    
    # Check if file should be ignored
    if should_ignore "$basename"; then
        echo "Skipping: $basename"
        continue
    fi
    
    # Remove existing file and create symlink
    target="$HOME_DIR/$basename"
    if [ -e "$target" ] || [ -L "$target" ]; then
        rm -rf "$target"
    fi
    
    ln -s "$file" "$target"
    echo "Linked: $basename"
done

# Link Cursor settings (macOS and WSL support)
echo -e "${YELLOW}Setting up Cursor editor...${NC}"
if [[ "$OSTYPE" == "darwin"* ]]; then
    # macOS
    src="$HOME_DIR/.cursor/settings.json"
    dst="$HOME_DIR/Library/Application Support/Cursor/User/settings.json"
    if [ -d "$(dirname "$dst")" ]; then
        [ -e "$dst" ] && rm -f "$dst"
        ln -s "$src" "$dst"
        echo "Cursor settings linked for macOS"
    else
        echo -e "${RED}Cursor directory not found on macOS${NC}"
    fi
elif grep -qEi "(WSL)" /proc/version 2>/dev/null; then
    # WSL
    if [ -n "$WINDOWS_HOME" ]; then
        src="$HOME_DIR/.cursor/settings.json"
        dst="$WINDOWS_HOME/AppData/Roaming/Cursor/User/settings.json"
        if [ -d "$(dirname "$dst")" ]; then
            cp "$src" "$dst"
            echo "Cursor settings copied for WSL"
        else
            echo -e "${RED}Cursor directory not found on Windows${NC}"
        fi
    else
        echo -e "${RED}\$WINDOWS_HOME not set for WSL${NC}"
    fi
fi

echo -e "${GREEN}Setup complete!${NC}"
echo ""
echo "Next steps:"
echo "1. Restart your shell or run: source ~/.zshrc"
echo "2. Run 'mise install' to install development tools"