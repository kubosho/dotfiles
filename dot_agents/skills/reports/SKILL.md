---
name: reports
description: |
  Report the work done across Herdr panes in this session as one Artifact page with a tab per pane: where each pane is, what it did, with diagrams, and the user's prompts and the replies about that work. Use this when the user asks for a report of parallel or delegated work, or says they lost track of which pane did what, including `/reports`, 「どの pane で何をやったかまとめて」「作業の報告を出して」「レポートにして」. Requires HERDR_ENV=1.
---

Gather what each Herdr pane did in this session and publish it as one Artifact page. Each pane gets a tab. A tab shows where the pane is, what it did with a diagram, what is left, and the user's prompts about that work with the replies.

## Steps

1. Check `test "${HERDR_ENV:-}" = 1`. If it fails, say that this session is not running inside Herdr and stop.
2. Collect the panes and their work. See "Sources".
3. Write one tab per pane. See "Tab content".
4. Build the page and publish it. See "The page".
5. Reply in chat with the link and the page title only.

## Sources

- Panes: every pane this session created, started an agent in, or sent a prompt to. Read them with `herdr agent list` and `herdr agent get <name>`. Take the agent name, pane ID, tab ID, workspace ID, cwd, and agent kind from the JSON, not from memory
- Work: the briefs and reports the agents wrote (scratchpad files), commits and branches in their worktrees (`git -C <cwd> log`), and external changes they reported (for example Figma node IDs). Read the files before quoting them
- Prompts and replies: the user's messages in this conversation and this session's replies to them. When the conversation was compacted, read the session transcript under `~/.claude/projects/<project>/<session-id>.jsonl` for the earlier part. Take only the user's own words, not tool results, system notices, or pasted material
- The main session itself is a tab too, for work it did without delegating (for example answering decisions or editing artifacts), when there is any

A prompt belongs to the tab whose work it asked for or answered. A prompt about several panes goes in each of those tabs. A prompt that belongs to none of them (small talk, a question about this skill) goes in the main session tab.

## Tab content

Order the tabs by when the work started. Each tab has these sections, in this order.

- Where: agent name, pane ID, tab ID, workspace ID, cwd, and branch when there is one. Do not show the agent's status (idle, working, and so on). The page cannot refresh it, and a status that does not update has no value
- What was done: one or two short paragraphs, then a diagram. Separate finished work, unverified points, and what is left. Point to evidence: commit hashes, file paths, node IDs with links
- Prompts and replies: a list in time order. Each item has the time, the user's prompt quoted as written, and the reply. Quote a short reply as written. Shorten a long reply to what it reported or asked, and say it was shortened

Write in the user's language and in plain words. Do not use labels or abbreviations coined during the work. Do not write counts of tabs, prompts, or items.

## Diagrams

Load `artifact-diagramming` before drawing. Draw with inline SVG. Mermaid blocks do not render on the page.

Draw what a sentence cannot: the flow between panes and the main session (who delegated what, which report came back), the before and after of a screen or a structure, the order of steps that depend on each other. Label every arrow. If a sentence says it faster, skip the diagram.

## The page

Load `artifact-design` before writing the file. Publish with the `Artifact` tool. The page needs no runtime capabilities.

- Title: a short noun phrase naming the session's work, not "Reports"
- Tabs follow the WAI-ARIA APG Tabs pattern with automatic activation
  - A `role="tablist"` with an accessible name, `role="tab"` buttons, and one `role="tabpanel"` per tab with `aria-labelledby` pointing at its tab
  - The selected tab has `aria-selected="true"` and `tabindex="0"`, the others `aria-selected="false"` and `tabindex="-1"`. Each tab has `aria-controls` pointing at its panel
  - Left and Right arrows move to the previous and next tab and wrap around. Home and End move to the first and last. Moving focus selects the tab
  - The panel has `tabindex="0"` when it has no focusable content at its start
  - Remember the selected tab in `localStorage` inside try/catch, and fall back to the first tab
- Each tab label shows the agent name, so the user can tell the panes apart without opening them
- Do not write text that explains what the screen already shows

When this skill runs again in the same session, republish the same file path so the link stays the same.
