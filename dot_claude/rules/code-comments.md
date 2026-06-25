---
paths:
  - "**/*.{ts,tsx,js,jsx,mjs,cjs}"
  - "**/*.{py,rb,go,rs,java,kt,swift}"
  - "**/*.{c,cc,cpp,h,hpp}"
  - "**/*.{sh,bash,zsh,lua}"
---

# Code Comments

Applies when adding or modifying code comments.

## Outcome

Comments record decisions the code itself cannot show. Future readers can reconstruct why a particular approach won.

## Constraints

- When the user presents multiple approaches and one is chosen, add a comment explaining why the other options were not selected.
- Comment language follows the convention defined in `vcs-commit-message.md` (match the last 5 commit logs).

## Invariants

### Comments explain, code shows

A comment that restates what the code does adds noise. Write comments that record motivation, trade-offs, or constraints the code cannot express on its own.
