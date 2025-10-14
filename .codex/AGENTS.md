# AGENTS settings

- Provide all answers in Japanese

## Agent Behavior

- Clean up temporary files upon task completion
- Execute independent operations in parallel
- Maintain workspace cleanliness
- Write code comments in English
- Do not write comments that are already obvious from the code or that simply indicate code removal

## Implementation Guidelines

### Commit Strategy

Follow a step-by-step commit approach:

- Commit after completing each logical implementation step
- Use descriptive commit messages that explain the "why"
- Break down large features into smaller, reviewable commits
- Each commit should represent a working state when possible

### Example Workflow

```shell
# Implement step 1, then commit
git add . && git commit -m "feat: add basic structure for new functionality"

# Implement step 2, then commit
git add . && git commit -m "feat: implement core logic"

# Continue until feature is complete
```
