# Claude Code Settings

## Agent Behavior

- Clean up temporary files upon task completion
- Do not write comments that are already obvious from the code or that simply indicate code removal
- Execute independent operations in parallel
- Write code comments in English

## Version Control

- Check if the repository uses jj (jujutsu) by looking for a `.jj` directory
  - If `.jj` directory exists: Use jj commands for version control operations
  - If `.jj` directory does not exist: Use git commands

