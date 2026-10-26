---
name: delegate-implementation
description: |
  Hand an implementation task to a Sonnet agent in a new Herdr pane, with a self-contained brief written to the scratchpad. Use this when the user asks to delegate, hand off, or have another pane's agent implement something, including `/delegate-implementation <task>`, 「別ペインのエージェントに実装を頼んで」「Herdr で Sonnet に任せて」「実装を委譲して」. Requires HERDR_ENV=1.
argument-hint: "<what to implement>"
---

Start a Sonnet agent in a sibling Herdr pane and give it a brief it can work from without this conversation. Follow the `herdr` skill for command details.

## Steps

1. Check that you run inside Herdr with `test "${HERDR_ENV:-}" = 1`. If it fails, say so and stop.
2. If `.jj` exists in the working directory, run `jj new` to put an empty change on top of the current working copy. The delegate's edits then stay out of the previous commit. In a git-only repository, skip this step and tell the user the delegate edits the working tree directly.
3. Start the agent in a new pane.
   1. Run `herdr pane layout --pane "$HERDR_PANE_ID"`. Split `right` when the pane is wide and `down` when it is tall.
   2. Run `herdr pane split --current --direction <right|down> --cwd "$PWD" --no-focus` and read the new pane ID from `.result.pane.pane_id`.
   3. Run `herdr agent start <name> --kind claude --pane <pane_id> --timeout 60000 -- --model sonnet`. Name the agent after the work, such as `campaign-impl`. It must match `[a-z][a-z0-9_-]{0,31}`.
4. Write the brief as Markdown in the scratchpad. See "Brief contents".
5. Hand it over.
   1. Run `herdr agent prompt <name> "<absolute brief path> を読んで、書かれている実装を進めてください。"`.
   2. Run `herdr agent get <name>` and confirm the state is `working`. If it is not, read `herdr agent read <name>` before sending anything else. Do not submit the prompt again blindly.
6. Report to the user. See "Report".

## Sandbox

Try the herdr commands inside the sandbox first. If a command fails on the herdr socket or a permission error, retry that command alone with the sandbox disabled, and mention it in the report.

## Brief contents

The delegate has no access to this conversation. Write the brief so it can work from the file alone.

- Repository path and the VCS state: current bookmark or branch, and that the working copy is a fresh empty change
- Background: the preceding changes and where the specs or API definitions live, as paths or URLs
- What to do: concrete steps, with the list of target files
- What not to change
- Commands to verify the work, such as tests, type checks, and lint
- How to split commits and the commit message format

Match the commit format to the rules in effect for the repository. Do not ask the delegate to push or open a PR unless the user asked for it.

## Report

Tell the user:

- The pane ID, agent name, and absolute brief path
- How to check progress: `herdr agent read <name>`
- Decisions placed in the brief that the user did not confirm, listed explicitly so they can be corrected
