---
name: decision-list
description: |
  List what the user has to decide, extracted from the recent conversation and from reports of delegated agents. Use this when the user asks to list, gather, or sort out the decisions they have to make, including `/decision-list`, 「判断が必要なことを一覧にして」「何を決めればいい？」「決めることをまとめて」「私が判断することは？」, or right after receiving the report of a delegated agent (for example a Sonnet agent in another Herdr pane) that mixes open questions, unverified points, and choices the agent made alone.
---

Collect the points where the user has to make a call, and show them as one list. The user makes the final decision, so the list gives the material for it and does not steer it.

## Steps

1. Gather the material. See "Sources".
2. Drop what does not need the user. See "Exclusions".
3. Write one entry per decision. See "Entry format".
4. Output the list and wait. Do not run, start, or prepare any item in the list before the user answers.

## Sources

Read the recent conversation and collect from these places.

- Questions and concerns the delegated agent or a tool reported: points it hesitated on, options it listed, things it asked about
- Choices made without asking the user, by the delegate or by yourself during the work. These look settled in the diff but the user has not agreed to them
- Unverified points: checks not run, behavior not confirmed, outputs not compared against the spec or design. Each becomes a decision about who verifies it and how
- What comes after the work: commit layout, push, PR creation, base branch, and who reviews

If the report is not in the conversation and the user points at a file or a pane, read it first. For a Herdr pane, use `herdr agent read <name>`.

## Exclusions

- Decisions the user has already made in this conversation. Do not reopen them
- Choices that follow the project's conventions or the rules in effect, where asking would only add noise

When unsure whether a choice follows a convention, list it. A missing decision costs more than an extra entry.

## Entry format

Each entry has a bold heading and bullets under it.

- Heading: what the decision is about, in a noun phrase the user recognizes without context
- Current state: what is the case now, in one or two bullets
- Options: each option with what happens if it is chosen. Include the cost that is easy to miss
- For an unverified point, write the options as who checks and how

Write in the user's language and in plain words. Do not use labels or abbreviations coined during the work. Do not write how many entries there are, in the heading or anywhere else, because the number goes stale when entries change.

Example of the shape, not a template for wording:

```markdown
**Commit layout**
- Now: split into two commits, font size and spacing
- Keep the split, or squash into one

**Visual check**
- Opening the page in a browser and comparing with Figma is not done yet
- Who does it: ask the delegate agent, or the user looks
```

## Recommendations

Do not write a recommendation, and do not word options so that one looks preferred. Keep the option descriptions even in length and tone. Give a recommendation only when the user asks for it, and then state the reason and the main downside.

## After the list

Wait for the user's answer. If the answer covers only some entries, act on those and keep the rest open.
