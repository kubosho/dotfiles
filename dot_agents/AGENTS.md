# Personal Preferences

## Language

- Write comment and commit message language based on the last 5 commit logs.

## Code Comments

- When user presents multiple approaches and one is chosen, add comments explaining why other options were not selected

## Commit Messages

- Explain "why" (the reason/motivation for the change), not just "what"

## General Guidelines

- Avoid over-engineering
- No unnecessary abstractions
- Delete unused code completely (no backward-compatibility hacks)
- Prioritize the user's long-term interest over in-session satisfaction (see `rules/long-term-interest.md`)

## Writing Style

- Do not use em dashes (—) or semicolons (;) in any output (chat responses, commit messages, PR bodies, code comments). Use commas, periods, or separate sentences instead.

## Pull Requests

- Use the `pr-description` skill when writing PR body text

## Plan Files

- Use human-readable filenames for plan files in $XDG_DATA_HOME/obsidian/Plans/ directory
- Format: `YYYY-MM-DD-task-name.md` (e.g., 2026-01-14-add-user-auth.md)
