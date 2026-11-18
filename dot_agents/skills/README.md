# Skills

[日本語](README.ja.md)

Agent skills deployed to `~/.agents/skills/` by chezmoi. Each directory holds a `SKILL.md` that defines when the skill is used and what it does.

## Work and delegation

| Skill | Description |
| --- | --- |
| [delegate-implementation](delegate-implementation/SKILL.md) | Hands an implementation task to a Sonnet agent in a new Herdr pane, with a self-contained brief written to the scratchpad. Requires `HERDR_ENV=1`. |
| [reports](reports/SKILL.md) | Reports the work done across Herdr panes as one Artifact page with a tab per pane, including diagrams and related prompts and replies. Requires `HERDR_ENV=1`. |
| [decision-list](decision-list/SKILL.md) | Lists what the user has to decide, taken from the recent conversation and from delegated agents' reports, as an Artifact page with a diagram and an answer field per decision. |
| [task-register](task-register/SKILL.md) | Creates a draft task file in Obsidian's `91_Tasks/Lists` from a description, for work to park instead of doing now. |
| [pr-screenshot](pr-screenshot/SKILL.md) | Checks UI changes on the actual screen, takes a screenshot per state, and attaches them to the PR with `gh --attach`. |

## Review and writing

| Skill | Description |
| --- | --- |
| [drunk-reviewer](drunk-reviewer/SKILL.md) | Code review in the voice of a mid-career developer reviewing a PR at 2am a few beers deep, posted as inline comments in difit. |
| [japanese-tech-writing](japanese-tech-writing/SKILL.md) | Rules for argument, restraint, and formatting in Japanese technical documents such as blog posts, design docs, and postmortems, including paragraph-level editing. |
| [information-architect](information-architect/SKILL.md) | Designs or audits information architecture (sitemap, taxonomy, metadata schema, content model, navigation, labeling) as a handoff to UX/UI designers. Used only when named explicitly or called from an agent. |

## Learning and thinking

| Skill | Description |
| --- | --- |
| [kabeuchi](kabeuchi/SKILL.md) | Interviews the user about a design or plan with one open question at a time, so the user puts the design reasons into their own words. |
| [protege-test](protege-test/SKILL.md) | Tests the user's understanding by having Claude play a curious beginner who asks the user to explain concepts from the recent conversation. |
| [english-coach](english-coach/SKILL.md) | Coaches English writing and reading on GitHub Issues and PRs at CEFR A2, giving structure, stock phrases, and hints on the user's own draft without writing it for them. |

## Diary

| Skill | Description |
| --- | --- |
| [write-diary](write-diary/SKILL.md) | Appends a work observation entry to today's AI diary in Obsidian. Used only on explicit request. |
| [diary-report](diary-report/SKILL.md) | Reviews AI diaries over a date range and creates a temporary standalone HTML report of patterns and possible next actions. |
