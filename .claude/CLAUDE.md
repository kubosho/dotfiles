# Claude Code Settings

Load and apply personality from @"~/.config/llm/personalities/shinosawa_hiro/prompt.md" before responding.

## Response Requirements

- Load personality file first, apply personality then respond
- Communicate in Japanese

## Agent Behavior

- Clean up temporary files upon task completion
- Execute independent operations in parallel
- Maintain workspace cleanliness

## Learning Mode

If requested "Add FIXME comments" or "I want to learn the code":

- Read and follow: @"~/.config/llm/ai_agents/teach-to-fish-agent.md"
- Maintain personality while teaching

## Implementation Guidelines

### Git Worktree Usage

When implementing features, use `git worktree` for isolated development:

- Use the available `wt-*` aliases from `.gitconfig` in home directory
- Create branches in separate worktrees: `git wt-create branch-name`
- Work in the `.git-worktrees/branch-name` directory
- This keeps the main working directory clean during development

### Commit Strategy

Follow a step-by-step commit approach:

- Commit after completing each logical implementation step
- Use descriptive commit messages that explain the "why"
- Break down large features into smaller, reviewable commits
- Each commit should represent a working state when possible

### Example Workflow

```shell
# Create feature worktree
git wt-create feature/new-functionality

# Work in worktree
cd .git-worktrees/feature/new-functionality

# Implement step 1, then commit
git add . && git commit -m "feat: add basic structure for new functionality"

# Implement step 2, then commit
git add . && git commit -m "feat: implement core logic"

# Continue until feature is complete
```